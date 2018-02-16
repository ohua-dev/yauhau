module Ohua.Transform.Yauhau where


import           Control.Comonad.Env               (env, runEnv)
import           Control.Monad.Reader              as R
import           Control.Monad.Writer
import           Data.Foldable
import           Data.Functor.Foldable
import           Data.Generics.Uniplate.Operations hiding (cata, para)
import qualified Data.HashMap.Strict               as HM
import qualified Data.HashSet                      as HS
import           Ohua.ALang.Lang
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
         => FnId -> Expression -> m ( (Binding -> ExprBuilder)
                                    , (Binding -> Binding -> ExprBuilder)
                                    , (Binding -> Expression)
                                    )
splitExp fnId expr = do
  ((buildSec1LastLet, buildSec2, buildSec3), Mutator buildSec1) <- runWriterT $ flip runReaderT mempty $ para go expr
  pure ( \bnd -> buildSec1 . buildSec1LastLet bnd
       , buildSec2
       , buildSec3
       )
  where
    go (LetF assign (val, _) (body, contBody))
      | sfHasId fnId val = do
          combInputs <- generateBinding
          combFreeBnd <- generateBinding
          allCombBnd <- generateBinding

          (rewordedExpr, bnds) <- rewordExpression val

          resultBnd <- generateBinding
          defined <- ask
          let carryoverFreeVars = HS.toList $ HS.intersection defined usedBindings
          tellMut $ Let (Direct combInputs) combExpr
          tellMut $ Let (Direct combFreeBnd) $ foldl' Apply mkTupSf (map (Var . Local) carryoverFreeVars)

          newCarryoverFreeVars <- traverse generateBindingWith carryoverFreeVars

          combFreeBnds1 <- generateBinding
          inputsBnd <- generateBinding
          combFreeBnds2 <- generateBinding
          resultBnd2 <- generateBinding

          pure
            ( \allCombBnd -> Let (Direct allCombBnd) $ mkTupSf `Apply` Var (Local combInputs) `Apply` Var (Local combFreeBnd)
            , \midSectionInput midSectionOut ->
                Let (Destructure [inputsBnd, combFreeBnds1]) (idSf `Apply` Var (Local midSectionInput))
                . Let (Destructure $ map snd bnds) (idSf `Apply` Var (Local inputsBnd))
                . Let (Direct resultBnd) rewordedExpr
                . Let (Direct midSectionOut) (idSf `Apply` Var (Local resultBnd) `Apply` Var (Local combFreeBnds1))
            , \lastSectionInput ->
                Let (Destructure [resultBnd2, combFreeBnds2]) (Var (Local lastSectionInput))
                $ Let assign (idSf `Apply` Var (Local resultBnd2))
                $ Let (Destructure newCarryoverFreeVars) (idSf `Apply` Var (Local combFreeBnds2))
                $ let lookupTable = HM.fromList $ zip carryoverFreeVars newCarryoverFreeVars
                  in cata (\case VarF (Local b) | Just b' <- HM.lookup b lookupTable -> Var (Local b'); other -> embed other) body
            )
      | otherwise = do
          tellMut $ Let assign val
          R.local (HS.union (HS.fromList $ flattenAssign assign)) contBody
        where
          (sf, combExpr) = renameSf val
          usedBindings = HS.fromList [ b | Var (Local b) <- universe body ]

    go _ = error "expected let"

tellMut :: MonadWriter (Mutator a) m => (a -> a) -> m ()
tellMut = tell . Mutator


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


