module Yauhau.Run where

import Monad.StreamsBasedFreeMonad
import Control.Monad.Free
import Data.Hashable
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Data.Typeable
import Control.Concurrent.MVar
import qualified Ohua.DFGraph as G
import qualified Ohua.Util.Str as Str
import qualified Text.PrettyPrint.Boxes as Box
import Data.Dynamic2
import Ohua.ALang.Lang (Expression)
import Ohua.Compile (passAfterDFLowering, passAfterNormalize, compile)
import Ohua.Transform.Yauhau
import Control.Monad.IO.Class
import Control.Monad.Except
import Ohua.Monad
import Data.Default.Class
import Ohua.Unit
import Ohua.ALang.Show
import Control.Concurrent.Async
import Lens.Micro.Platform
import Data.Maybe
import Data.Vector (Vector)
import Type.Magic
import Data.List (transpose)

type Tree a = Free [] a

type ResultVar = MVar

newResultVar :: IO (ResultVar a)
newResultVar = newEmptyMVar

putResultVar :: ResultVar a -> a -> IO ()
putResultVar = putMVar

class DataSource req where
  answer :: [WaitingRequest req] -> IO ()

data WaitingRequest req = forall a . WaitingRequest (req a) (ResultVar a)

data WReq
  = forall req a
  . ( DataSource req
    , Typeable a
    , Typeable (req a)
    , Hashable (req a)
    , Typeable req
    , Eq (req a))
  => WReq (req a)

type RequestTree = Tree WReq
type ResponseTree = Tree Dynamic

fetchSf :: Sf (Tree WReq -> SfMonad () (Tree Dynamic))
fetchSf = Sf f (Just fetchName)
  where
    f (Pure (WReq r)) = sfm $ liftIO $ do
      resVar <- newResultVar
      -- this could also be spawned in parallel
      answer [WaitingRequest r resVar]
      pure $ Pure $ toDyn resVar
    f _ = error "Invariant broken, got Free, expected Pure"

simpleLift f = call (liftSf $ sfm . pure . f) united
simpleLift2 f = call (liftSf $ \a b -> sfm $ pure $ f a b) united


dataFetch :: ( DataSource req
             , Typeable a
             , Typeable (req a)
             , Hashable (req a)
             , Typeable req
             , Eq (req a))
          => Var (req a)
          -> ASTM s (Var a)
dataFetch req = do
  packaged <- simpleLift (Pure . WReq) req
  answered <- call fetchSf united packaged
  call (liftSf $ sfm . liftIO . readMVar . forceDynamic . unpure) united answered
  where
    unpure (Pure v) = v
    unpure _        = error "Invariant broken, got Free, expected Pure"

runCompilerY :: Expression -> IO G.OutGraph
runCompilerY
  = fmap (either (error . Str.toString) makeDestructuringExplicit)
  . runExceptT
  . runStderrLoggingT
  . filterLogger (const $ (>= LevelError))
  . compile def def { passAfterDFLowering = cleanUnits
                    , passAfterNormalize = \alang' -> do
                        alang <- giveFetchesIds alang'
                        liftIO $ putStrLn $ unlines
                          [ ""
                          , "Before transform:"
                          , ""
                          , Box.render (renderExpr alang)
                          , ""
                          ]
                        lifted <- liftSmap alang
                        liftIO $ putStrLn $ unlines
                          [ ""
                          , "After Lift:"
                          , ""
                          , Box.render (renderExpr lifted)
                          , ""
                          ]
                        combined <- combineIO lifted
                        liftIO $ putStrLn $ unlines
                          [ ""
                          , "After combine:"
                          , ""
                          , Box.render (renderExpr combined)
                          , ""
                          ]
                        pure combined
                    }

answerRequests :: (Eq request, Hashable request)
               => ([request] -> IO [response])
               -> [Tree request] -> IO [Tree response]
answerRequests answer trees = do
  responses <- answer reqList
  let responseMap = HM.fromList (zip reqList responses)
  pure $ map (fmap (responseMap HM.!)) trees
  where
    reqList = HS.toList $ HS.fromList $ trees >>= foldFree id


data DSScopedMap
  = forall req
  . (Typeable req, DataSource req)
  => DSScopedMap (HM.HashMap TypeRep (ReqScopedMap req))


data ReqScopedMap req
  = forall a
  . (Typeable a, Typeable (req a))
  => ReqScopedMap (HM.HashMap (req a) (MVar a))


accumHandle :: Vector (Tree WReq) -> IO (Vector (Tree Dynamic))
accumHandle reqs = do
  allReqs <- foldM createAndInsert HM.empty reqList
  mapM_ (\(DSScopedMap reqs) -> async $ answer $ flattenDsScopedMap reqs) (HM.elems allReqs)
  pure $ fmap (fmap $ lookupResVar allReqs) reqs
  where
    reqList = V.toList reqs >>= foldFree id

    getRTy :: Typeable t => t a -> TypeRep
    getRTy = typeOf1
    getRaTy :: Typeable a => a -> TypeRep
    getRaTy = typeOf

    lookupResVar :: HM.HashMap TypeRep DSScopedMap -> WReq -> Dynamic
    lookupResVar m (WReq (r :: r a)) = fromMaybe (error "Invariant broken, request not found") $ do
      DSScopedMap m0 <- m ^? ix rty
      ReqScopedMap m1 <- m0 ^? ix raTy
      case cast m1 of
        Just m2 -> toDyn <$> (m2 :: HM.HashMap (r a) (MVar a) ) ^? ix r
        Nothing -> error "failed the type cast"
      where
        rty = getRTy r
        raTy = getRaTy r

    flattenDsScopedMap :: HM.HashMap TypeRep (ReqScopedMap req) -> [WaitingRequest req]
    flattenDsScopedMap m =
      [ wr
      | x <- HM.elems m
      , wr <- case x of
                ReqScopedMap m -> map (uncurry WaitingRequest) $ HM.toList m
      ]

    createAndInsert :: HM.HashMap TypeRep DSScopedMap -> WReq -> IO (HM.HashMap TypeRep DSScopedMap)
    createAndInsert reqMap (WReq (r :: req a)) =
      case reqMap ^? ix rty of
        Nothing -> do
          resVar <- newResultVar
          pure $ insertReqMap (newDSScopedMap resVar)
        Just (DSScopedMap dsScopedMap_)
          | Just dsScopedMap <- cast dsScopedMap_ ->
            let insertDsScopedMap val = insertReqMap (DSScopedMap $ dsScopedMap & at raTy .~ Just val)
            in
              case (dsScopedMap :: HM.HashMap TypeRep (ReqScopedMap req)) ^? ix raTy of
                Nothing -> do
                  resVar <- newResultVar
                  pure $ insertDsScopedMap (newReqScopedMap resVar)
                Just (ReqScopedMap reqScopedMap_)
                  | Just reqScopedMap <- cast reqScopedMap_ ->
                    case reqScopedMap ^? ix r of
                      Nothing -> do
                        resVar <- newResultVar
                        pure $ insertDsScopedMap (ReqScopedMap $ reqScopedMap & at r .~ Just resVar)
                      Just var -> pure reqMap
                  | otherwise -> error "Cast failed"
          | otherwise -> error "Cast failed"
          where
            
      where
        rty = getRTy r
        raTy = getRaTy r

        insertReqMap val = reqMap & at rty .~ Just val

        newDSScopedMap = DSScopedMap . HM.singleton raTy . newReqScopedMap
        newReqScopedMap = ReqScopedMap . HM.singleton r
      


createAlgoY :: ASTM s (Var a) -> IO (Algorithm s a)
createAlgoY astm = do
  gr <- runCompilerY expr
  -- let simpleStrLabel = pure . toLabel
  --     dotParams = nonClusteredParams
  --       { fmtNode = \(id, label) -> simpleStrLabel $ show label ++ "(" ++ show id ++ ")"
  --       , fmtEdge = \(_, _, OhuaGrEdgeLabel source target) -> simpleStrLabel $ show source ++ " -> " ++ show target
  --       }
  --     dotGr = graphToDot dotParams (unGr $ toFGLGraph gr)
  -- fp <- runGraphviz dotGr Jpeg "testgr.jpeg"
  -- putStrLn $ "Graphviz return was " ++ fp
  pure $ graphToAlgo dict' gr
  where
    (dict, expr) = evaluateAST astm
    dict' = dict
            & at accumName .~ Just (StreamProcessor $ pure $ do
                                       vals <- fmap forceDynamic <$> recieveAll
                                       withIsAllowed $
                                         send =<< liftIO (accumHandle vals)
                                       )
            & at mkTupRef .~ Just (StreamProcessor $ pure $ withIsAllowed . send =<< recieveAllUntyped)
            & at unzipName .~ Just (StreamProcessor $ pure $ do
                                       ls <- recieve 0 :: StreamM [V.Vector Dynamic]
                                       withIsAllowed $
                                         send $ V.fromList $ map injectList $ transpose $ map V.toList ls
                                   )
            & at zipName .~ Just (StreamProcessor $ pure $ do
                                     ls <- map extractList . V.toList <$> recieveAllUntyped
                                     withIsAllowed $
                                       send $ map V.fromList $ transpose ls
                                 )
            & at mkTreeName .~ Just (pureSF $ (Free :: [RequestTree] -> RequestTree))
            & at unTreeName .~ Just (pureSF $ (\case Free l -> l :: [ResponseTree]; _ -> error "Invariant broken, expected Free, got pure"))
    pureSF f = CallSf $ SfRef (liftSf (sfm . pure . f)) united
