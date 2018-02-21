import           Data.Graph.Inductive.Graph
import           Data.Graph.Inductive.PatriciaTree
import           Ohua.DFGraph
import           Ohua.Test.DFGraph
import           Ohua.Types
import           Test.Hspec


import qualified Data.IntMap.Strict                as IntMap
import qualified Data.IntSet                       as IntSet


matchAndReport :: (Eq a, Ord b, Show a, Show b) => Gr a b -> Gr a b -> IO ()
matchAndReport gr1 gr2 =
      -- TODO add a check here to verify that all nodes have unique function IDs
--matchAndReport g1 g2 =
--    let gr1 = trace ("Graph #1: " ++ show g1) g1
--        gr2 = trace ("Graph #2: " ++ show g2) g2 in
      case matchGraph gr1 gr2 of
        Right _ -> return ()
        Left (largest, keys) ->
            let selectedGr1Nodes = IntMap.elems largest
                selectedGr2Nodes = IntMap.keys largest
                unselectedGr1Nodes = filter (not . flip IntSet.member (IntSet.fromList selectedGr1Nodes) . fst) (labNodes gr1)
                unselectedGr2Nodes = filter (not . flip IntSet.member (IntMap.keysSet largest) . fst) (labNodes gr2)
            in
                expectationFailure $ unlines
                    [ "Graphs weren't isomorphic."
                    , "The largest match was between"
                    , ""
                    , prettify (subgraph selectedGr1Nodes gr1)
                    , ""
                    , "and"
                    , ""
                    , prettify (subgraph selectedGr2Nodes gr2)
                    , ""
                    , "I could not match the nodes"
                    , ""
                    , show $ unselectedGr1Nodes
                    , ""
                    , "with"
                    , ""
                    , show $ unselectedGr2Nodes
                    , case keys of
                        Nothing -> ""
                        Just (_, x) -> unlines
                            [ ""
                            , "I failed when trying to match"
                            , show $ filter ((== x) . fst) unselectedGr1Nodes
                            , "with the edges:"
                            , show $ filter (\(a, b, _) -> a==x || b==x) (labEdges gr1)
                            , "With any of: "
                            , show unselectedGr2Nodes
                            ]
                    ]


main :: IO ()
main = hspec $ do return ()
