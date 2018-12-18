import Data.List
import Data.Char

-- This Solution is copied from https://www.youtube.com/watch?v=oydhA3TmB-0&t=1290s
-- You will need to subtract 1 from the result of the computation. Somehow it adds 1 to the result...
-- wasnt able to find the reason why that is
type Input = String 

main :: IO()
main = do x <- readFile "input.txt"
          print . show . exec $ x

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

shortestPathByRemovingOneChar i = minimum . map (\a -> (length . collapse) (filter (not . sameLetter (head a)) i)) . groupBy sameLetter . sortBy compareLetter $ i
 
