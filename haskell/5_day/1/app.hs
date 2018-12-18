import Data.List
import Data.Char

type Input = String 

main :: IO()
main = do x <- readFile "input.txt"
          print . show . exec $ x

exec :: String -> Int
exec = length . collapse 

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
