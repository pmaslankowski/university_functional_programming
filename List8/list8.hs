import qualified Data.Set as DataSet
import qualified Data.Map as DataMap
import Data.Set (Set)
import Data.Map (Map, (!))
import Control.Monad

-- Exercise 1:
newtype OptGraph a = OptGraph (Int, Int -> Set Int, Int -> a, a -> Int)
newtype ListGraph a = ListGraph ([a], a -> [a])

class Graph g where 
  nodes :: Ord a => g a -> Set a
  
  neighbors :: Ord a => g a -> a -> Set a
  
  opt :: Ord a => g a -> OptGraph a
  opt graph = OptGraph (n, e, l, i)
    where nodesSet = nodes graph
          n        = DataSet.size nodesSet
          l        = \x -> DataSet.elemAt x nodesSet
          i        = \x -> DataSet.findIndex x nodesSet
          e        = \x -> DataSet.map i (neighbors graph $ l x)
  
  graph_eq :: (Eq a, Ord a) => g a -> g a -> Bool
  graph_eq g1 g2 = 
    let nodesList1 = DataSet.toList $ nodes g1
        nodesList2 = DataSet.toList $ nodes g2
        edgePairs  = map (\x -> (neighbors g1 x, neighbors g2 x)) nodesList1
    in (nodes g1 == nodes g2) && all (\(x, y) -> x == y) edgePairs

instance Graph OptGraph where
  nodes (OptGraph (n, e, l, i)) = DataSet.fromList $ map l [0..n-1]

  neighbors (OptGraph (n, e, l, i)) x = DataSet.map l (e $ i x)

  opt x = x

instance Graph ListGraph where
  nodes (ListGraph (vs, edges)) = DataSet.fromList vs

  neighbors (ListGraph (vs, edges)) x = DataSet.fromList $ edges x


-- TESTS
test1 = ListGraph ("abcdef", \x -> case x of 'a' -> "bc"
                                             'b' -> "c"
                                             'c' -> "df"
                                             'd' -> "ef"
                                             'e' -> "a"
                                             'f' -> "" )

test2 = ListGraph("abcdef", \x -> case x of 'a' -> "cb"
                                            'b' -> "c"
                                            'c' -> "df"
                                            'd' -> "ef"
                                            'e' -> ""
                                            'f' -> "" )


-- Exercise 2:
print_graph :: (Ord a, Show a, Graph g) => g a -> IO ()
print_graph graph =
  do print $ nodes graph 
     sequence [ print $ DataSet.toList $ neighbors graph x | x <- DataSet.toList $ nodes graph]
     return ()

get_graph :: Ord a => (String -> a) -> IO (ListGraph a)
get_graph f = let readEdges x = do neighs <- getLine
                                   return (x, map f $ read neighs) 
              in do nodesStr <- getLine
                    nodes <- return $ map f $ read nodesStr
                    edgesList <- mapM readEdges nodes
                    edgesMap <- return $ DataMap.fromList edgesList 
                    return $ ListGraph (nodes, \x -> edgesMap ! x)
                    
-- for testing purposes
main :: IO ()
main = get_graph id >>= print_graph
                 