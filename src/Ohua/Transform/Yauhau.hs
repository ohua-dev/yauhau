module Ohua.Transform.Yauhau where


import           Control.Comonad.Env               (env, runEnv)
import           Control.Monad.Reader              as R
import           Control.Monad.Writer
import           Data.Foldable
import           Data.Functor.Foldable
import           Data.Generics.Uniplate.Operations hiding (para)
import qualified Data.HashMap.Strict               as HM
import qualified Data.HashSet                      as HS
import           Ohua.ALang.Lang
import           Ohua.ALang.Passes                 (normalize)
import qualified Ohua.ALang.Refs                   as Refs
import           Ohua.Monad
import           Ohua.Types
import           Ohua.Util


mkTupRef :: QualifiedBinding
mkTupRef = "ohua.lang/mkTup"


findFreeVars :: Expression -> HS.HashSet Binding
findFreeVars = flip runReader mempty . cata go
  where
    go (VarF (Local b)) = do
      isDefined <- R.asks (HS.member b)
      pure $ if isDefined
             then mempty
             else HS.singleton b
    go (LambdaF assign body) = regAssign assign body
    go l@(LetF assign _ _) = regAssign assign $ generic l
    go other = generic other

    generic = fmap (foldl' HS.union mempty) . sequence

    regAssign = R.local . HS.union . HS.fromList . flattenAssign


type ExprBuilder = Expression -> Expression


splitExp :: (MonadGenBnd m, MonadError Error m)
         => FnId -> HS.HashSet Binding -> Expression
         -> m ( Expression
              , Expression
              , (Binding -> Binding -> Expression -> ExprBuilder)
              , [Binding]
              , (Binding -> Binding -> Expression)
              )
splitExp fnId predefinedBnds expr = do
  ((buildSec1LastLet, e, buildSec2, vars, buildSec3), Mutator buildSec1) <- runWriterT $ flip runReaderT predefinedBnds $ para go expr
  pure ( buildSec1 buildSec1LastLet
       , e
       , buildSec2
       , vars
       , buildSec3
       )
  where
    go (LetF assign (val, _) (body, contBody))
      | sfHasId fnId val = do
          combInputs <- generateBinding
          combFreeBnd <- generateBinding

          (rewordedExpr', bnds) <- rewordExpression val

          resultBnd <- generateBinding
          defined <- ask
          let carryoverFreeVars = HS.toList $ HS.intersection defined usedBindings
          tellMut $ Let (Direct combInputs) combExpr
          tellMut $ Let (Direct combFreeBnd) $ foldl' Apply mkTupSf (map (Var . Local) carryoverFreeVars)

          newCarryoverFreeVars <- traverse generateBindingWith carryoverFreeVars

          combFreeBnds1 <- generateBinding
          inputsBnd <- generateBinding

          pure
            ( mkTupSf `Apply` Var (Local combInputs) `Apply` Var (Local combFreeBnd)
            , rewordedExpr'
            , \midSectionInput midSectionOut rewordedExpression ->
                Let (Destructure [inputsBnd, combFreeBnds1]) (idSf `Apply` Var (Local midSectionInput))
                . Let (Destructure $ map snd bnds) (idSf `Apply` Var (Local inputsBnd))
                . Let (Direct resultBnd) rewordedExpression
                . Let (Direct midSectionOut) (idSf `Apply` Var (Local resultBnd) `Apply` Var (Local combFreeBnds1))
            , newCarryoverFreeVars
            , \resultBnd2 combFreeBnds2 ->
                Let assign (idSf `Apply` Var (Local resultBnd2))
                $ Let (Destructure newCarryoverFreeVars) (idSf `Apply` Var (Local combFreeBnds2))
                $ let lookupTable = HM.fromList $ zip carryoverFreeVars newCarryoverFreeVars
                  in cata (\case VarF (Local b) | Just b' <- HM.lookup b lookupTable -> Var (Local b'); other -> embed other) body
            )
      | otherwise = do
          tellMut $ Let assign val
          R.local (HS.union (HS.fromList $ flattenAssign assign)) contBody
        where
          (_, combExpr) = renameSf val
          usedBindings = HS.fromList [ b | Var (Local b) <- universe body ]

    go _ = error "expected let"


liftSmap :: MonadOhua env m => Expression -> m Expression
liftSmap = cata $ \case
  LetF assign var rest -> var >>= \case
    Apply (Apply (Var (Sf sf _)) (Lambda lassign lbody)) someArg
      | sf == Refs.smap -> do
          (lam, cont) <- go assign lassign lbody
          r' <- rest
          normalize $ lam someArg $ cont r'
    v -> Let assign v <$> rest
  e -> embed <$> sequence e

  where
    go finalAssign lassign lbody
      | Just i <- findFetchId lbody = do
          (before, _, _, _, buildEnd) <- splitExp i (HS.fromList $ flattenAssign lassign) lbody
          vals <- generateBindingWith "fetches"
          envs <- generateBindingWith "envs"
          endEnv <- generateBindingWith "env"
          endVar <- generateBindingWith "fetch"

          let newLAssign = Destructure [endVar, endEnv]
          let lamBody = buildEnd endVar endEnv

          (lam, cont) <- go finalAssign newLAssign lamBody

          let newE :: ExprBuilder
              newE e =
                lam (zipSF `Apply` (fetchSF `Apply` Var (Local vals)) `Apply` (Var (Local envs)))
                $ cont e
          pure
            ( \arg -> Let (Destructure [vals, envs]) (unzipSF `Apply` (smapSF `Apply` Lambda lassign before `Apply` arg))
            , newE
            )
      | otherwise = pure (Let finalAssign . Apply (Lambda lassign lbody), id)


findFetchId :: Expression -> Maybe FnId
findFetchId e = case [ i | Var (Sf ty i) <- universe e, ty == Refs.smap ] of
                  Just x:_  -> Just x
                  Nothing:_ -> error "found fetch with no id"
                  _         -> Nothing


fetchSF, zipSF, unzipSF, smapSF :: Expression
fetchSF = Var $ Sf "yauhau/fetch" Nothing
zipSF = Var $ Sf "yauhau/zip" Nothing
unzipSF = Var $ Sf "yauhau/unzip" Nothing
smapSF = Var $ Sf Refs.smap Nothing


mkTupSf, idSf :: Expression
mkTupSf = Var (Sf mkTupRef Nothing)
idSf = Var (Sf Refs.id Nothing)


sfHasId :: FnId -> Expression -> Bool
sfHasId i = cata $ \case
  ApplyF cont _ -> cont
  VarF (Sf _ sfid) -> sfid == Just i
  _ -> error "Expected Var of Apply"


renameSf :: Expression -> (ResolvedSymbol, Expression)
renameSf = runEnv . para go
  where
    go = \case
      ApplyF (_, w) (v, _) -> flip Apply v <$> w
      VarF s@(Sf _ _) -> env s mkTupSf
      _ -> error "Expected apply or sf var"


rewordExpression :: (MonadError Error m, MonadGenBnd m) => Expression -> m (Expression, [(Binding, Binding)])
rewordExpression = runWriterT . para go
  where
    go = \case
      ApplyF (_, e) (Var v, _) ->
        Apply <$> e <*> (Var <$> case v of
                                   Local b -> do
                                     b' <- generateBindingWith b
                                     tell [(b, b')]
                                     pure $ Local b'
                                   other -> pure other)
      VarF v -> pure $ Var v
      _ -> throwError $ "Expected var or apply"


