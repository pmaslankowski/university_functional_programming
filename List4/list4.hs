{- Programowanie funkcyjne, lista 4
   Grupa kzi,
   Piotr Maślankowski -}
import Debug.Trace
import Data.Sequence ((<|), (|>), (><))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Control.Seq 


--Exercise 1:
newtype Graph a = Graph (a -> [a], [a])



--Exercise 2:
newtype Step a = Step (a, a) deriving Show
instance (Eq a) => Eq (Step a) where
    Step x == Step y = snd x == snd y

{-}
short_path :: (Show a, Eq a) => Graph a -> a -> a -> Maybe [a]
short_path (Graph (g, _)) start end =
    let
        visit v visited []     = if v `elem` visited then visited
                                                     else visit v (v:visited) vertices
                                                     where Step (_, vertex) = v
                                                           vertices         = map (\x ->Step (vertex, x)) $ g vertex
        visit v visited (q:qs) | v == Step (undefined, end) = v:visited
                               | v `elem` visited = visit q visited qs
                               | otherwise        = visit q (v:visited) (qs ++ vertices)
                                                    where Step (_, vertex) = v
                                                          vertices         = map (\x -> Step (vertex, x)) $ g vertex

        startStep              = Step (start, start)

        buildPath ((Step (from, curr)):steps) v acc
               | curr == start = acc
               | curr == v     = buildPath steps from (from:acc)
               | otherwise     = buildPath steps v acc

        steps                  = visit startStep [] [startStep]

        Step (from, last)      = head steps
  in if last == end then Just $ buildPath steps last [last]
                    else Nothing

-}

--exercise 2:
short_path (Graph (e, v)) start end = if start == end then Just [start] else
  let bfs queue visited acc
            | queue == Seq.empty = acc
            | otherwise = bfs queue' visited' acc'
                          where curr Seq.:< queueTail = Seq.viewl queue
                                neighbors = filter (\x -> Set.notMember x visited) $ e curr
                                endNow    = if end `elem` neighbors then True else False
                                queue'    = if endNow then Seq.empty else queueTail >< (Seq.fromList neighbors) 
                                acc'      = if endNow then Map.insert end curr acc 
                                                      else foldr (\x xs -> 
                                                                  if   Map.notMember x xs 
                                                                  then Map.insert x curr xs 
                                                                  else xs) 
                                                            acc neighbors
                                visited'  = Set.insert curr visited

      buildPath prevs v acc = if v == start then v:acc else buildPath prevs (prevs Map.! v) (v:acc)
      prevs = bfs (Seq.singleton start) Set.empty Map.empty
      
  in if Map.member end prevs then Just $ buildPath prevs end [] else Nothing

graph1 :: Graph Integer
graph1 = Graph (\x -> case x of
          1 -> [2, 3]
          2 -> [3]
          3 -> [4, 6]
          4 -> [2, 5]
          5 -> [6]
          6 -> [5], [1,2,3,4,5,6])


--exercise 3:
ham_cycle :: Eq a => Graph a -> Maybe [a]
ham_cycle (Graph  (_, [])) = Just []
ham_cycle (Graph (e, v:vs)) =
  let remove _ []      = []
      remove el (x:xs) = if x == el then xs else x:remove el xs
      
      safeHead [] = Nothing
      safeHead (x:_) = Just x

      paths v u (Graph (e, [_])) = if v == u then [[v]] else []
      paths v u (Graph (e, vs))  = do
                              w <- e v
                              p <- paths w u g'
                              return (v:p)
                                where g' = Graph (\x -> remove v $ e x, remove v vs)
      
      g' = Graph (\x -> v:(e x), v:v:vs)
  in safeHead $ paths v v g'

graph2 = Graph (\x -> case x of
                          1 -> [2]
                          2 -> [3, 4]
                          3 -> [4,1]
                          4 -> [1, 5]
                          5 -> [1], [1,2,3,4,5])


--exercise 4:
{-
rev (Graph (es, vs)) = Graph ((\v -> map fst $ filter (\el -> Set.member v $ snd el) listOfSets), vs)
                         where listOfSets = map (\x -> (x, Set.fromList $ es x)) vs -}

rev (Graph (es, vs)) = 
  let neighbors v             = foldr (\x acc -> if v `elem` (es x) then x:acc else acc ) [] vs
      edgesMap                = map (\x -> (x, neighbors x)) vs
      find v ((x, neighs):xs) = if x == v then neighs else find v xs
  in Graph (\v -> find v edgesMap, vs)


graph3 = Graph(\x -> case x of
                         1 -> []
                         2 -> [1,3]
                         3 -> [4, 5]
                         4 -> []
                         5 -> [1], [1,2,3, 4, 5])

--exercise 5:
data Tree a = Node { val :: a, chl :: [Tree a]} deriving Show
{-
tree_of :: Eq a => Graph a -> Maybe (Tree a)
tree_of (Graph (es, vs)) =
  let tree_from visited v = if v `elem` visited then Nothing 
                                                else case subtrees of
                                                       Nothing -> Nothing
                                                       Just x -> Just $ (Node { val = v, chl = map fst x }, v:concatMap snd x)  
        where 
          unHat (Just x)       = x
          maybeToBool Nothing  = False
          maybeToBool (Just _) = True
          possibleTrees        = map (tree_from (v:visited)) $ es v
          subtrees             = if any (not.maybeToBool) possibleTrees then Nothing 
                                                                        else Just $ map unHat possibleTrees
          
      aux [] = Nothing
      aux (x:xs) = case curr of
                     Nothing -> aux xs
                     Just (tree, verticesInTree) -> if length verticesInTree == length vs then Just tree else aux xs
                     where curr = tree_from [] x 
  in aux vs 
-}

tree_of (Graph (es, vs)) =
  let 
    Graph (revEs, _)     = rev (Graph (es, vs))
    candidates           = filter (\x -> revEs x == []) vs
    tree_from visited v  = if v `elem` visited then Nothing 
                                               else case subtrees of
                                                      Nothing -> Nothing
                                                      Just x -> Just $ (Node { val = v, chl = map fst x }, v:concatMap snd x)  
      where 
        unHat (Just x)       = x
        maybeToBool Nothing  = False
        maybeToBool (Just _) = True
        possibleTrees        = map (tree_from (v:visited)) $ es v
        subtrees             = if any (not.maybeToBool) possibleTrees then Nothing 
                                                                      else Just $ map unHat possibleTrees
  in 
  case candidates of
      [] -> Nothing
      [x] -> tree_from [] x
      _ -> Nothing