{-# LANGUAGE TemplateHaskell, OverloadedLists, FlexibleContexts #-}
module Yauhau.Transform where

import ClassyPrelude
import Control.Lens hiding (snoc)
import qualified Control.Lens as Lens
import Yauhau.IR.Utils
import Control.Monad.State (State, execState, execStateT, modify, get, put, MonadState)
import Data.Maybe (fromJust)
import Data.List (findIndex)


data RewriteState = RewriteState
    { _nameGen :: [Text]
    , _graph :: IRGraph
    , _labelMap :: IntMap CtxStack
    , _idGen :: [Int]
    } deriving (Show, Eq)

data GraphPosition = GraphPosition
    { _visited :: HashSet IRFunc
    , _stage :: HashSet IRFunc
    , _round :: HashSet IRFunc
    , _dependencies :: HashSet Binding
    } deriving (Show, Eq)

makeLenses ''GraphPosition

makeFields ''RewriteState

reindexPreserving :: [Arg] -> [Arg]
reindexPreserving = flip execState 0 . mapM go
    where go item = do
                maybe (return ()) put $ item^.argIndex
                i <- get
                modify succ
                return $ item & argIndex .~ Just i


reindexStuff :: [Arg] -> [Arg]
reindexStuff = flip execState 0 . mapM go
    where go item = do
              i <- modify succ
              return $ item & argIndex .~ Just i

mapEither f g = either (Left . f) (Right . g)

mkFuncAndIndex :: Integer -> Text -> [Arg] -> [Arg] -> IRFunc
mkFuncAndIndex id_ name_ args_ return_ =
    IRFunc id_ name_ (reindexStuff args_) (mapEither (argIndex .~ Just -1) reindexStuff return_)

class ToFnID a where
    toFnID :: a -> Int

instance ToFnID Int where toFnID = id
instance ToFnID IRFunc where toFnID = (^.identifier)

canonicalizeLabelKey = toFnID
stateMkName = do
    (h:rest) <- use nameGen
    nameGen .= rest
    return h

type Rewrite a = State RewriteState a

putLabel key label = labelMap . at key .= Just label
unsafeModifyGraph f =  graph %= f
stateDeleteFn fn = graph %= filter (/= fn)
stateReplaceNodes :: (HasGraph state, MonadState state m, IsMap map) => map -> (Maybe CtxStack) -> m ()
stateReplaceNodes replaces label = 
    graph %= concatMap (\f -> fromMaybe [f] $ lookup f replaces)
    maybe (return ()) (flip stateLabelAllWith $ map snd (toList replaces)) label
stateReplaceNode node replace = stateReplaceNodes $ singletonMap node replace 
stateGenId = do
    (h:rest) <- use idGen
    idGen .= rest
    return h
stateMkFunc name_ args_ return_ = (\i -> mkFuncAndIndex i name_ args_ return_) <$> stateGenId
stateMkFuncUnindexed name_ args_ return_ = (\i -> IRFunc i name_ args_ return_) <$> stateGenId
stateMkNames n = do
    ng <- use nameGen
    let (names, rest) = splitAt n ng
    nameGen .= rest
    return names
stateLabelAllWith label_ fns =
    labelMap %= (++ mapFromList (zip fns (repeat label_)))
stateDeleteLabel l = labelMap . at l .= Nothing
stateDeleteLabels ls = mapM stateDeleteLabel ls
stateGetLabel :: (HasLabelMap state, MonadState state m, ToFnID f) => f -> m CtxStack
stateGetLabel l = use $ labelMap.ix (toFnID l)
returnBindingsList = either return id . (^.returnBindings)
stateFindFunc f = find ((== toFnID f) . toFnID) <$> use graph

isFetch = (== "fetch") . (^.name)

firstGraphPosition gr = GraphPosition gr (setFromList (filter (null . (^.argumentBindings)) gr)) mempty mempty
moveForward = do
    curr <- use stage
    let newDeps = curr >>= returnBindingsList
    dependencies %= (++ newDeps)
    visited %= (++curr)
    visit
  where
    visit = do
        deps <- use dependencies
        vis <- use visited
        gr <- use graph
        stage .~ filter (\fn -> not (visited^.contains fn) && all (`member` dependencies) (returnBindingsList fn)) gr

findNextFetches = do
    curr <- use stage
    if null curr
        then return []
        else
            let (io, nonIO) = partition isFetch curr
            in do
                stage .= nonIO
                moveForward
                (io ++) <$> findNextFetches


batchRewrite :: Rewrite ()
batchRewrite = do
    graph_ <- use graph
    execStateT (firstGraphPosition graph_) $ go graph_
  where
    go ogr = do
        fetches <- findNextFetches
        if null fetches
            then do
                vis <- use visited
                assert $ all (`member` visited) ogr
            else do
                let inputs = fetches >>= (^.argumentBindings)
                    outputs = fetches >>= returnBindingsList
                accum <- stateMkFunc "yauhau.function/__accum-fetch" inputs outputs
                visited %= (++ [accum])
                graph %= filter (not . (`member` fetches))
                graph %= (++ [accum])
                dependencies %= (++ setFromList outputs)
                go ogr

topmostIfFrame = fromJust . find ((== "if") . (^.ctxType))

stateHasTopmostIfFrame func ifOp = do
    l <- use $ labelMap.at (toFnID func)
    return $ case topmostIfFrame l of
                Nothing -> False
                Just foundIf -> foundIf^.opId == ifOp

stateMkEmptyRq ctxArg = do
    n <- Arg Nothing <$> stateMkName
    stateMkFunc "yauhau.functions/__empty-request" [Arg (Just -1) ctxArg] (Left n)

genEmptyFor amount in_ = do
    req <- stateMkEmptyRq
    let reqOut = req^?!returnBindings._Left
    inserts <- sequence (replicate amount (stateMkName >>= stateMkFunc "yauhau.function/fetch" [Arg Nothing reqOut] . Left . Arg Nothing))
    return (req:inserts)

getFetchesConcerned ifOp = use graph >>= filterM go
    where go func
              | isFetch func = return False
              | otherwise = stateHasTopmostIfFrame func ifOp

stateMatchingIfFrame :: (HasLabelMap state, MonadState state m, ToFnID f) => f -> f -> m CtxFrame
stateMatchingIfFrame ifOp func = do
    l <- stateGetLabel func
    return $ fromJust $ find (\f -> f^.ctxType == "if" && f^.opId == (toFnID ifOp)) l

mappedFetchesForIf = undefined


insertEmpties :: IRFunc -> [IRFunc] -> Rewrite ()
insertEmpties ifOp fetchesConcerned = do
    mappedToPort <- mappedFetchesForIf ifOp
    let [short, long] = sortOn (length . snd) $ mapToList mappedToPort
        longL = length (snd long)
        shortL = length (snd short)
        toGen = long - short
    unless (toGen == 0) $ do
        let ifPort = short^._1
        shortenedIfStack <- stateGetLabel ifOp
        lastStackEntry <- stateMatchingIfFrame (ifOp^.identifier) (headEx $ snd long)
        let outV = findIndex ((==ifPort) . (^.binding)) (ifOp^?!returnBindings._Right)
            ifStack = shortenedIfStack `snoc` (lastStackEntry & outVar .~ outV)
        generated <- genEmptyFor toGen ifPort
        stateLabelAllWith ifStack generated

ifRewriteOne :: CtxFrame -> Rewrite ()
ifRewriteOne CtxFrame{_opId = ifOp, _outVar = idx} = do
    label <- stateGetLabel ifOp
    currIf <- stateFindFunc ifOp
    let trueOut = head $ curr^.returnBindings 
    directFetchesConcerned <- getFetchesConcerned ifOp
    insertEmpties ifOp directFetchesConcerned
    mappedToPort <- mappedFetchesForIf ifOp

    for_ 
        (zip (fromJust $ lookup 0 mappedToPort) (fromJust $ lookup 1 mappedToPort))
        $ \(f1, f2) -> do
            newLabel <- statePopMatchingIfFrame ifOp f1
            let inputs = f1^.argumentBindings ++ f2 ^.argumentBindings
            [mergeOut, fetchOut] <- stateMkNames 2
            mergeFn <- stateMkFunc "com.ohua.lang/select" $ trueOut : inputs
            newFetch <- stateMkFunc "yauhau.functions/fetch" [Arg (Just 0) mergeOut] (Left $ Arg (Just -1) fetchOut)
            identityOps <- for [f1, f2] $ \fun -> do
                frame <- stateMatchingIfFrame ifOp fun
                stateMkFunc "identity" [returnBindingsList ifOp & at idx.argIndex .~ Just -1, Arg (Just 0) fetchOut] (fun^.returnBindings)
            stateDeleteFn f2
            stateReplaceNode f1 (mergeFn:newFetch:identityOps) (Just newLabel)


unwindContext triggerMap irGraph labelMap = do

  where
    presentNames = setFromList $ graph >>= \fn -> map (^.binding) (fn^.argumentBindings ++ returnBindingsList fn)
    nameGen = [ ident
              | num <- "": map show [0..]
              , char <- ["a".."z"]
              , let ident = char ++ show num
              , not $ ident `member` presentNames
              ]
    presentIds = setFromList $ graph^..folded.identifier
    idGen = [ nid
            | nid <- [0..]
            , not $ nid `member` presentIds
            ]
    labelStack = irGraph
        & filter isFetch
        & filter (\f -> maybe (error "no label found") (not . null) (lookup f labelMap))
    workingOrder = undefined