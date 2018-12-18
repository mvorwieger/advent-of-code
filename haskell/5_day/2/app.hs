import Data.List
import Data.Char

-- This Solution is copied from https://www.youtube.com/watch?v=oydhA3TmB-0&t=1290s

type Input = String 

main :: IO()
main = do x <- readFile "input.txt"
          print . show . exec $ x

exec :: String -> Int
exec = shortestPathByRemovingOneChar 

doesReact :: Char -> Char -> Bool
doesReact a b = a /= b && (a == toUpper b || b == toUpper a)

next :: Input -> Input
next (x:y:xs) | doesReact x y = next xs
              | otherwise = x : next (y:xs)
next x@(_) = x

collapse :: Input -> Input
collapse s | s == s' = s
           | otherwise = collapse s'
    where s' = next s

compareLetter :: Char -> Char -> Ordering
compareLetter a b = toUpper a `compare` toUpper b

sameLetter :: Char -> Char -> Bool
sameLetter a b = toUpper a == toUpper b

shortestPathByRemovingOneChar :: String -> Int
shortestPathByRemovingOneChar i = minimum . map (\a -> exec (filter (not . sameLetter (head a)) i)) . groupBy sameLetter . sortBy compareLetter $ i
