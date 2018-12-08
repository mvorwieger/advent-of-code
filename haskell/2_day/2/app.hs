main :: IO()
main = do
    file <- readFile "./input.txt"
    let linesOfFile = lines file
    putStr $ execute linesOfFile

input = [
      "abcde"
    , "fghij"
    , "klmno"
    , "pqrst"
    , "fguij"
    , "axcye"
    , "wvxyz"
    ]

execute input = snd $ head $ filter (\ a -> fst a == 1) $ mconcat $ map (\a -> map (compareString a) input) input

type Difference = Int

compareString :: String -> String -> (Difference, String)
compareString s1 s2 = compS s1 s2 (0, "")
                        where
                            compS (h1:s1) (h2:s2) (dif, s) = if h1 == h2 
                                                        then compS s1 s2 (dif, (s ++ [h1]))
                                                        else compS s1 s2 (dif + 1, s)
                            compS [] _ s = s
                            compS _ [] s = s

