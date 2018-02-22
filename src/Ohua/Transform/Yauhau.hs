module Ohua.Transform.Yauhau where


import           Control.Arrow
import           Control.Comonad.Env               (env, runEnv)
import           Control.Monad.Reader              as R
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Foldable
import           Data.Functor.Foldable
import           Data.Generics.Uniplate.Operations hiding (contexts, para)
import           Data.Graph.Inductive.Graph
import           Data.Graph.Inductive.PatriciaTree
import qualified Data.HashMap.Strict               as HM
import qualified Data.HashSet                      as HS
import qualified Data.IntSet                       as IntSet
import           Data.Maybe
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


fetchName :: QualifiedBinding
fetchName = "yauhau/fetch"


fetchSF, zipSF, unzipSF, smapSF :: Expression
fetchSF = Var $ Sf fetchName Nothing
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


-- The yauhau node label type
data YNL
  = SfL QualifiedBinding (Maybe FnId)
  | EnvL HostExpr
  | LambdaL Assignment Expression

-- A yauhau edge label
type YEL = Maybe Binding

type DepGr = Gr QualifiedBinding ()


buildDependencyGraph :: DynGraph gr => Expression -> gr QualifiedBinding ()
buildDependencyGraph = flip execState empty . flip runReaderT mempty . cata go
  where
    askSource b = asks $ join . HM.lookup b . fst
    askDeps = asks snd
    addDeps = local . second . maybe id (:)
    registerSources bnds src = local $ first $ \hm -> foldr' (\k -> HM.insertWith (error "duplicate source") k src) hm bnds

    go (VarF v) =
      case v of
        Local b -> askSource b
        Sf _ Nothing -> error "No id"
        Sf sf (Just (FnId sfid)) -> do
          deps <- askDeps
          modify $ insNode (sfid, sf)
          modify $ insEdges $ map (sfid, , ()) deps
          pure $ Just sfid
        _ -> pure Nothing
    go (ApplyF fn val) = do
      valDeps <- val
      addDeps valDeps fn
    go (LetF assign val body) = do
      source <- val
      registerSources (flattenAssign assign) source body
    go (LambdaF _ _) = pure Nothing


focusGraph :: (Monoid b, DynGraph gr) => (LNode a -> Bool) -> gr a b -> gr a b
focusGraph p gr = foldl' go gr $ map fst $ filter p $ labNodes gr
  where
    go gr' n = insEdges [ (source, target, a `mappend` b)
                        | (source, a) <- lpre' ctx
                        , (target, b) <- lsuc' ctx
                        ]
                        gr'
      where
        ctx = context gr' n

fetchDependencyGraph :: (DynGraph gr, Monoid b) => gr QualifiedBinding b -> gr QualifiedBinding b
fetchDependencyGraph = focusGraph (\(_, sf) -> sf == fetchName)


contexts :: Graph gr => gr a b -> [Context a b]
contexts = ufold (:) []


calculateFetchRounds :: Graph gr => gr a b -> [IntSet.IntSet]
calculateFetchRounds gr = unfold go initials
  where
    initials = IntSet.fromList $ map node' $ filter (null . suc') $ contexts gr
    go vertices | IntSet.null new = Nil
                | otherwise = Cons new new
      where
        new = IntSet.fromList $ IntSet.toList vertices >>= suc gr

-- TODO improve:
  -- there's a case where this transformation fails
  -- This inserts the accumulator after the *last* expression that produces a value
  -- a participating fetch depends on. However the graph we calculate before
  -- does not respect the sequentiality of expressions.
  -- it may therefore be possible that the result of a fetch is used before the last
  -- dependency for the accumulator is produced.
  -- This is because we suse the original expression, but we must in some cases reorder it.
  -- I opt to ignore this problem for now.
transform :: MonadGenBnd m => Expression -> m Expression
transform e = foldl' rewriteRound e rounds
  where
    gr = buildDependencyGraph e
    fetchGr = fetchDependencyGraph gr
    rounds = calculateFetchRounds fetchGr

    insertAcumExpr round rest = do
      
      Let rest
      where 

    rewriteRound e' round = flip runReader mempty $ flip runStateT False $ para go e
      where
        generic = fmap embed . traverse snd
        deps = IntSet.fromList $ IntSet.toList round >>= pre gr
        go e''@(LetF assign (Apply (Sf sf (Just i)) arg, val) (_, body)) = do
          isDep <- asks $ IntSet.member i

          if isDep
            then local (IntSet.delete i) $ do
              shouldInsert <- asks IntSet.null
              if shouldInsert
                then do
                  v <- val
                  b <- body
                  insertAcumExpr round =<< Let assign v b
                else Let assign <$> val <*> body
            else generic e''
        go e'' = generic e''

