module Ohua.Transform.Yauhau where


import           Control.Category
import           Control.Comonad
import           Control.Comonad.Cofree
import           Control.Comonad.Env               (env, runEnv)
import           Control.Monad.Reader              as R
import           Control.Monad.Writer
import           Data.Foldable
import           Data.Functor.Contravariant        (Predicate (..))
import           Data.Functor.Foldable
import           Data.Generics.Uniplate.Operations hiding (contexts, para)
import qualified Data.HashMap.Strict               as HM
import qualified Data.HashSet                      as HS
import           Lens.Micro
import           Lens.Micro.Mtl                    hiding (assign)
import           Ohua.ALang.Lang
import           Ohua.ALang.Passes                 (normalize)
import qualified Ohua.ALang.Refs                   as Refs
import           Ohua.Monad
import           Ohua.Types
import           Ohua.Util
import           Ohua.Util.Str                     (showS)
import           Prelude                           hiding (id, (.))


mkTupRef :: QualifiedBinding
mkTupRef = "ohua.lang/mkTup"


findFreeVars :: Expression -> HS.HashSet Binding
findFreeVars = ($ mempty) . cata go
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


fetchName, accumName, zipName, unzipName :: QualifiedBinding

fetchName = "yauhau/fetch"
accumName = "yauhau/accum"
zipName =  "yauhau/zip"
unzipName = "yauhau/unzip"

fetchSF, zipSF, unzipSF, smapSF :: Expression
fetchSF = Var $ Sf fetchName Nothing
zipSF = Var $ Sf zipName Nothing
unzipSF = Var $ Sf unzipName Nothing
smapSF = Var $ Sf Refs.smap Nothing


mkTupSf, idSf :: Expression
mkTupSf = Var (Sf mkTupRef Nothing)
idSf = Var (Sf Refs.id Nothing)


equals :: Eq a => a -> Predicate a
equals = Predicate . (==)


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
      _ -> throwErrorS $ "Expected var or apply"


groupFns :: MonadError Error m => Predicate QualifiedBinding -> Expression -> m Expression
groupFns p'@(Predicate p) = flip runReaderT (id, id, id, HS.empty) . go
  where
    go (Let assign val rest) = do
      ((sf, _), deps) <- decomp val
      sat <- areSatisfied deps
      let push | sat && p sf = pushTarget assign
               | sat = pushSat
               | otherwise = pushUnsat assign
      push (Let assign val) $ go rest
    go end@(Var (Local _)) = do
      (buildSat, buildTarget, buildUnsat, deps) <- ask

      lift $ fmap (buildSat . buildTarget) $ (if HS.null deps then pure else groupFns p') (buildUnsat end)
    go e = throwErrorS $ "Exprected let or var, got " <> showS e

    areSatisfied deps = asks ((\hs -> all (not . flip HS.member hs) deps) . view _4)
    appendLetOn l f = local (l %~ (. f))
    registerUnsatDeps assign = local (_4 %~ flip (foldr' HS.insert) (flattenAssign assign))
    pushSat = appendLetOn _1
    pushTarget assign mkLet = registerUnsatDeps assign . appendLetOn _2 mkLet
    pushUnsat assign mkLet = registerUnsatDeps assign . appendLetOn _3 mkLet

    decomp = runWriterT . para go0
      where
        go0 (ApplyF (_, fn) (Var val, _)) = do
          case val of
            Local b -> tell [b]
            _       -> pure ()
          fn
        go0 (VarF (Sf sf sfid)) = pure (sf, sfid)
        go0 e = throwErrorS $ "Expected apply or var, got " <> showS (embed $ fmap fst e)


-- | INVARIANT: The calls that are combined must each have a single input and a single return
combineTo :: forall m . MonadError Error m => Predicate QualifiedBinding -> QualifiedBinding -> Expression -> m Expression
combineTo p'@(Predicate p) f = histo go
  where
    recurse = combineTo p' f
    rebuild = ana unwrap
    go :: AExprF Binding ResolvedSymbol (Cofree (AExprF Binding ResolvedSymbol) (m Expression)) -> m Expression
    go (LetF assign1
         valC
         (_ :< LetF assign2
                    (_ :< ApplyF (_ :< VarF (Sf f' _)) arg)
                    rest))
      | isAccum acc && p f' =
        case (assign1, assign2) of
          (Destructure bnds, Direct b) -> recurse $ Let (Destructure (bnds ++ [b])) (val `Apply` rebuild arg) $ rebuild rest
          _ -> throwErrorS "Invariant broken. Assigns for combine 1st case."
      where
        acc = extractSf val
        val = rebuild valC

    go (LetF assign1
         (_ :< ApplyF (_ :< VarF (Sf f0 sfid)) arg1)
         (_ :< LetF assign2
                    (_ :< ApplyF (_ :< VarF (Sf f1 _)) arg2)
                    rest))
      | p f0 && p f1 =
          case (assign1, assign2) of
            (Direct bnd0, Direct b) ->
              recurse $ Let (Destructure [bnd0, b]) (Var (Sf f sfid) `Apply` rebuild arg1 `Apply` rebuild arg2) $ rebuild rest
            _ -> throwErrorS "Invariant broken. Assigns for combine 2nd case."

    go (LetF assign val body) = Let assign (rebuild val) <$> extract body
    go (VarF v) = pure $ Var v
    go e = throwErrorS $ "Expected let or var, got " <> showS (embed $ fmap rebuild e)

    isAccum = (== f)

extractSf :: Expression -> QualifiedBinding
extractSf = cata $ \case
  ApplyF fn _ -> fn
  VarF (Sf sf _) -> sf
  e -> error $ "Exprected apply or var, got " <> showS e


normalizeAssigns :: (Monad m, MonadGenBnd m) => Predicate QualifiedBinding -> Expression -> m Expression
normalizeAssigns (Predicate p) = cata $ \case
  LetF assign@(Destructure bnds) val body -> do
    val' <- val
    if p $ extractSf val'
       then do
         bnd <- generateBinding
         Let (Direct bnd) val' . Let (Destructure bnds) (Var (Sf Refs.id Nothing) `Apply` Var (Local bnd)) <$> body
       else Let assign val' <$> body
  other -> embed <$> sequence other

combineIO :: (MonadError Error m, MonadGenBnd m) => Expression -> m Expression
combineIO = normalizeAssigns isFetch >=> groupFns isFetch >=> combineTo isFetch accumName
  where isFetch = equals fetchName
