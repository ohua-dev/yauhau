{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}


import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
import           Data.Default.Class
import           Data.Dynamic2
import           Data.Function
import           Data.List
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Ord
import           Data.Time
import           Data.Typeable
import           GHC.Exts              (Any)
import           Lens.Micro
import           Ohua.ALang.Lang       hiding (Sf)
import           Ohua.Compile
import qualified Ohua.DFGraph          as G
import           Ohua.Monad
import           Ohua.Transform.Yauhau
import           Ohua.Unit
import qualified Ohua.Util.Str         as Str
import           Prelude               hiding (sequence)
import           StreamsBasedFreeMonad
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
dataFetch = call (Sf (sfm . pure . fetchFunc) (Just fetchName)) united

fetchFunc :: Request a -> a
fetchFunc = \case
  FetchPosts -> map PostId [0..10]
  FetchPostContent (PostId i) -> "[content " ++ show i ++ "]"
  FetchPostInfo i -> PostInfo i (UTCTime (toEnum 0) (toEnum 0)) ""
  FetchPostViews (PostId i) -> 0


simpleLift f = call (liftSf $ sfm . pure . f) united
simpleLift2 f = call (liftSf $ \a b -> sfm $ pure $ f a b) united

-- -----------------------------------------------------------------------------
-- Blog code

data Html = Html

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
renderPage _ _ = Html

renderPosts :: [(PostInfo, PostContent)] -> Html
renderPosts _ = Html

renderSidePane :: Html -> Html -> Html
renderSidePane _ _ = Html

renderPostList :: [(PostInfo, PostContent)] -> Html
renderPostList _ = Html

renderTopics :: Map String Int -> Html
renderTopics _ = Html

runCompilerY :: Expression -> IO G.OutGraph
runCompilerY
  = fmap (either (error . Str.toString) makeDestructuringExplicit)
  . runExceptT
  . runStderrLoggingT
  . filterLogger (const $ (>= LevelError))
  . compile def def { passAfterDFLowering = cleanUnits, passAfterNormalize = liftSmap >=> combineIO }

createAlgoY :: ASTM s (Var a) -> IO (Algorithm s a)
createAlgoY astm = graphToAlgo dict' <$> runCompilerY expr
  where
    (dict, expr) = evaluateAST astm
    dict' = dict
            & at accumName .~ Just (StreamProcessor $ pure $ do
                                       vals <- recieveAllUntyped
                                       send $ lToTup $ map (toDyn . fetchFunc . forceDynamic) vals
                                       )
            & at mkTupRef .~ Just (StreamProcessor $ pure $ send . lToTup =<< recieveAllUntyped)
            & at unzipName .~ Just (StreamProcessor $ pure $ do
                                       Dynamic ty val <- recieveUntyped 0
                                       let newVal = unzip (unsafeCoerce val ::[(Any, Any)])
                                       let newTy = mkTyConApp tupCon [mkTyConApp listCon [valATy], mkTyConApp listCon [valBTy]]
                                             where
                                               (listCon, [elemTy]) = splitTyConApp ty
                                               (tupCon, [valATy, valBTy]) = splitTyConApp elemTy
                                       sendUntyped $ Dynamic newTy (unsafeCoerce newVal)
                                   )
            & at zipName .~ Just (StreamProcessor $ pure $ do
                                     Dynamic ty1 val1 <- recieveUntyped 0
                                     Dynamic ty2 val2 <- recieveUntyped 1
                                     let newVal = zip (unsafeCoerce val1 :: [Any]) (unsafeCoerce val2 :: [Any])
                                     let newTy = mkTyConApp listCon [mkTyConApp (typeRepTyCon (typeRep (Proxy :: Proxy ((), ())))) [valATy, valBTy]]
                                           where
                                             (listCon, [valATy]) = splitTyConApp ty1
                                             [valBTy] = typeRepArgs ty2
                                     sendUntyped $ Dynamic newTy (unsafeCoerce newVal)
                                 )

main = flip runAlgo () =<< createAlgoY blog
