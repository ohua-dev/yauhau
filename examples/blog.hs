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
import           Yauhau.Run

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

instance DataSource Request where
  answer reqs = forM_ reqs $ \(WaitingRequest r var) ->
    putResultVar var $ fetchFunc r

    where
      fetchFunc :: Request a -> a
      fetchFunc FetchPosts = map PostId [0..10]
      fetchFunc (FetchPostInfo i) = PostInfo i (UTCTime (toEnum 0) (toEnum 0)) "t"
      fetchFunc (FetchPostContent i) = "[content " ++ show i ++ "]"
      fetchFunc (FetchPostViews (PostId i)) = (i `mod` 3) * 3

-- -----------------------------------------------------------------------------
-- Blog code

type Html = B.Html

type Fetch a = ASTM () (Var a)

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

main = putStrLn . B.renderHtml =<< flip runAlgo () =<< createAlgoY blog
