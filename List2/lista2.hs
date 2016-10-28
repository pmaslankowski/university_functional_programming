{- Piotr Maślankowski
   Programowanie funkcyjne, lista 2
   grupa kzi
-}

type Aut = ([Char], Int, Int -> Char -> Int, Int -> Bool)

autWords :: Aut -> [(String, Bool)]
autWords (alphabet, initState, autFunc, isAccepted) =
    let nextWords words = concatMap (\x -> map (x:) words) alphabet
        nextStates states = concatMap (\x -> map (\y -> autFunc x y) alphabet) states
        {- funkcja buduje słowa długości n+1 ze zbioru słów długości n -}
        concatIterate f a = x ++ (concatIterate f x) where x = f a
        generateWords = [[]] ++ concatIterate nextWords [[]]
        generateAutomataStates = [initState] ++ concatIterate nextStates [initState]
        {- funkcja tworzy listę stanów automatu dla odpowiednich słów w porządku leksykograficznym. 
         - Generuje ona słowo czytane od prawej do lewej. Tworząc kolejne słowo dodajemy tylko literę
         - na jego końcu, a następnie wyliczamy stan na podstawie starego stanu oraz tej litery. -}  
    in
        zipWith (\x y -> (x, isAccepted y)) generateWords generateAutomataStates
        {-
        generateAutomataStates = inverted where
            inverted = [initState] ++ concatMap (\x -> map (\y -> autFunc x y) alphabet) inverted-}

acceptedWords :: Aut -> [String]
acceptedWords aut = map (\(x, _) -> x) $ filter (\(_,y) -> y) $ autWords aut

testAutomata :: Aut
testAutomata = (['a', 'b'], 0, \_ c -> if c == 'a' then 0 else 1, \i-> i == 1)
