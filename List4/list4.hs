{- Programowanie funkcyjne, lista 4 
   Grupa kzi,
   Piotr Maślankowski -}

newtype Graph a = Graph (a -> [a])

newtype Step a = Step (a, a) deriving Show
instance (Eq a) => Eq (Step a) where
	Step x == Step y = snd x == snd y

graph1 :: Graph Integer
graph1 = Graph (\x -> case x of
					1 -> [2, 3]
					2 -> [3]
					3 -> [4, 6]
					4 -> [2, 5]
					5 -> []
					6 -> [5])

short_path :: Eq a => Graph a -> a -> a -> Maybe [a]
short_path (Graph g) start end =
    let visit v visited []     = visited 
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

