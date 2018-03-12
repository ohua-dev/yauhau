{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}


import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Free
import           Data.Default.Class
import           Data.Dynamic2
import           Data.Function
import           Data.GraphViz
import           Data.List
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import           Data.Ord
import qualified Data.Text.IO                    as T
import           Data.Time
import           Data.Typeable
import qualified Data.Vector                     as V
import           GHC.Exts                        (Any)
import           Lens.Micro
import           Monad.StreamsBasedFreeMonad
import           Ohua.ALang.Lang                 hiding (Sf)
import           Ohua.ALang.Show                 (renderExpr)
import           Ohua.Compile
import qualified Ohua.DFGraph                    as G
import           Ohua.DFGraph.Show
import           Ohua.Monad
import           Ohua.Test.DFGraph
import           Ohua.Transform.Yauhau
import           Ohua.Unit
import qualified Ohua.Util.Str                   as Str
import           Prelude                         hiding (sequence)
import qualified Text.Blaze.Html                 as B
import qualified Text.Blaze.Html.Renderer.Pretty as B
import qualified Text.Blaze.Html5                as B
import qualified Text.PrettyPrint.Boxes          as Box
import           Type.Magic
import           Types
import           Unsafe.Coerce

-- -----------------------------------------------------------------------------
-- IO

getPostIds :: Fetch [PostId]
getPostInfo :: Var PostId -> Fetch PostInfo
getPostContent :: Var PostId -> Fetch PostContent
getPostViews :: Var PostId -> Fetch Int

-- <<dataFetches
getPostIds     = dataFetch =<< call (liftSf $ sfm $ pure FetchPosts) united
getPostInfo    = dataFetch <=< simpleLift FetchPostInfo
getPostContent = dataFetch <=< simpleLift FetchPostContent
getPostViews   = dataFetch <=< simpleLift FetchPostViews
-- >>

dataFetch :: Typeable a => Var (Request a) -> Fetch a
dataFetch req = do
  packaged <- simpleLift (Pure . packageRequest) req
  answered <- call fetchSf united packaged
  unpackage <- simpleLift selectUnpackager req
  simpleLift2 ($) unpackage =<< simpleLift unpure answered
  where
    unpure (Pure v) = v
    unpure _        = error "Invariant broken, got Free, expected Pure"

fetchSf :: Sf (RequestTree -> SfMonad state ResponseTree)
fetchSf = Sf (sfm . pure . fmap fetchFunc) (Just fetchName)

fetchFunc :: Request__ -> Response__
fetchFunc = \case
  ReqPosts__ -> RespPosts__ $ map PostId [0..10]
  ReqContent__ (PostId i) -> RespContent__ $ "[content " ++ show i ++ "]"
  ReqInfo__ i -> RespInfo__ $ PostInfo i (UTCTime (toEnum 0) (toEnum 0)) ""
  ReqViews__ (PostId i) -> RespViews__ 0

data Request__
  = ReqPosts__
  | ReqInfo__ PostId
  | ReqContent__ PostId
  | ReqViews__ PostId
  deriving Typeable

data Response__
  = RespPosts__ [PostId]
  | RespInfo__ PostInfo
  | RespContent__ PostContent
  | RespViews__ Int
  deriving Typeable

selectUnpackager :: Request a -> Response__ -> a
selectUnpackager FetchPosts           = selectPostsResp
selectUnpackager (FetchPostContent _) = selectContentResponse
selectUnpackager (FetchPostInfo _)    = selectInfoResponse
selectUnpackager (FetchPostViews _)   = selectViewsResponse

selectPostsResp :: Response__ -> [PostId]
selectPostsResp (RespPosts__ p) = p
selectPostsResp _               = error "Expected posts response"

selectInfoResponse :: Response__ -> PostInfo
selectInfoResponse (RespInfo__ i) = i
selectInfoResponse _              = error "Expected info response"

selectContentResponse :: Response__ -> PostContent
selectContentResponse (RespContent__ c) = c
selectContentResponse _                 = error "Expected content response"

selectViewsResponse :: Response__ -> Int
selectViewsResponse (RespViews__ v) = v
selectViewsResponse _               = error "Expected views response"

packageRequest :: Request a -> Request__
packageRequest FetchPosts           = ReqPosts__
packageRequest (FetchPostInfo i)    = ReqInfo__ i
packageRequest (FetchPostContent c) = ReqContent__ c
packageRequest (FetchPostViews v)   = ReqViews__ v

simpleLift f = call (liftSf $ sfm . pure . f) united
simpleLift2 f = call (liftSf $ \a b -> sfm $ pure $ f a b) united

-- -----------------------------------------------------------------------------
-- Blog code

type Html = B.Html

type Fetch a = ASTM () (Var a)

type RequestTree = Free [] Request__
type ResponseTree = Free [] Response__

-- <<blog
blog :: Fetch Html
blog = do
  lp <- leftPane
  rp <- mainPane
  simpleLift2 renderPage lp rp
-- >>

-- <<getAllPosts
getAllPostsInfo :: Fetch [PostInfo]
getAllPostsInfo = smap getPostInfo =<< getPostIds
-- >>

-- <<getPostDetails
getPostDetails :: Var PostId
               -> Fetch (PostInfo, PostContent)
getPostDetails pid = do
  i <- getPostInfo pid
  ct <- getPostContent pid
  simpleLift2 (,) i ct
-- >>

-- <<leftPane
leftPane :: Fetch Html
leftPane = do
  pp <- popularPosts
  tp <- topics
  simpleLift2 renderSidePane pp tp
-- >>

-- <<topics
topics :: Fetch Html
topics = do
  posts <- getAllPostsInfo
  let topiccounts posts =
        Map.fromListWith (+)
          [ (postTopic p, 1) | p <- posts ]
  simpleLift renderTopics =<< simpleLift topiccounts posts
-- >>

-- <<popularPosts
popularPosts :: Fetch Html
popularPosts = do
  pids <- getPostIds
  views <- smap getPostViews pids
  let order pids views =
        take 5 $ map fst $
        sortBy (flip (comparing snd))
               (zip pids views)
  ordered <- simpleLift2 order pids views
  content <- smap getPostDetails ordered
  simpleLift renderPostList content
-- >>

-- <<mainPane
mainPane :: Fetch Html
mainPane = do
  posts <- getAllPostsInfo
  let order posts =
        take 5 $
        sortBy (flip (comparing postDate)) posts
  ordered <- simpleLift order posts
  content <- smap (getPostContent <=< simpleLift postId) ordered
  simpleLift renderPosts
    =<< simpleLift2 zip ordered content
-- >>

-- -----------------------------------------------------------------------------
-- Dummy rendering

renderPage :: Html -> Html -> Html
renderPage html1 html2 = B.html $ do
  B.head $ pure ()
  B.body $ do
    html1
    html2

renderPosts :: [(PostInfo, PostContent)] -> Html
renderPosts = renderPostList

renderSidePane :: Html -> Html -> Html
renderSidePane html1 html2 = do
  B.h3 "Side pane"
  html1
  html2

renderPostList :: [(PostInfo, PostContent)] -> Html
renderPostList l = do
  B.h3 "Postlist"
  B.ul $ forM_ l $ \(info, content) -> do
    B.li $ B.toHtml $ take 30 content

renderTopics :: Map String Int -> Html
renderTopics m = do
  B.h3 "Topics"
  B.table $ B.tbody $
    forM_ (Map.toList m) $ \(name, views) -> B.tr $ do
      B.td (B.toHtml name)
      B.td (B.toHtml views)

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

createAlgoY :: ASTM s (Var a) -> IO (Algorithm s a)
createAlgoY astm = do
  gr <- runCompilerY expr
  let simpleStrLabel = pure . toLabel
      dotParams = nonClusteredParams
        { fmtNode = \(id, label) -> simpleStrLabel $ show label ++ "(" ++ show id ++ ")"
        , fmtEdge = \(_, _, OhuaGrEdgeLabel source target) -> simpleStrLabel $ show source ++ " -> " ++ show target
        }
      dotGr = graphToDot dotParams (unGr $ toFGLGraph gr)
  fp <- runGraphviz dotGr Jpeg "testgr.jpeg"
  putStrLn $ "Graphviz return was " ++ fp
  pure $ graphToAlgo dict' gr
  where
    (dict, expr) = evaluateAST astm
    dict' = dict
            & at accumName .~ Just (StreamProcessor $ pure $ do
                                       vals <- recieveAllUntyped
                                       let processed = map (toDyn . (fmap fetchFunc :: RequestTree -> ResponseTree) . forceDynamic) vals
                                       send $ V.fromList $ processed
                                       )
            & at mkTupRef .~ Just (StreamProcessor $ pure $ send . V.fromList =<< recieveAllUntyped)
            & at unzipName .~ Just (StreamProcessor $ pure $ do
                                       ls <- recieve 0 :: StreamM [V.Vector Dynamic]
                                       send $ V.fromList $ map injectList $ transpose $ map V.toList ls
                                   )
            & at zipName .~ Just (StreamProcessor $ pure $ do
                                     ls <- map extractList <$> recieveAllUntyped
                                     send $ map V.fromList $ transpose ls
                                 )
            & at mkTreeName .~ Just (pureSF $ (Free :: [RequestTree] -> RequestTree))
            & at unTreeName .~ Just (pureSF $ (\case Free l -> l :: [ResponseTree]; _ -> error "Invariant broken, expected Free, got pure"))
    pureSF f = CallSf $ SfRef (liftSf (sfm . pure . f)) united

main = putStrLn . B.renderHtml =<< flip runAlgo () =<< createAlgoY blog
