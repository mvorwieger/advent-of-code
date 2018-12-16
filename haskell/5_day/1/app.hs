import Data.List
import Data.Char

main :: IO()
main = do
    str <- readFile "./input.txt"
    print . show . length . exec $ str 

doesReact :: Char -> Char -> Bool
doesReact a b = (toLower a == toLower b) && 
                ((isUpper a && isLower b) || (isLower a && isUpper b))  

reactingPairLeft :: String -> Bool
reactingPairLeft "" = False
reactingPairLeft (x:"") = False
reactingPairLeft (x:y:xs) 
     | doesReact x y = True
     | otherwise = reactingPairLeft (y:xs)

removePolymerRecursive :: String -> String
removePolymerRecursive x = let newString = filter (\a -> length a < 2) . groupBy doesReact $ x
           in if reactingPairLeft newString 
              then removePolymerRecursive newString
              else newString
