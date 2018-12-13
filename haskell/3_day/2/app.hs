import Data.List
import Control.Arrow 
import Data.Char
import Text.Parsec
import Text.Parsec.Char (spaces, digit, char)
import Text.Parsec.String (Parser)
import Text.Parsec (parse, many1, ParseError)

type Cord = (Int, Int)
type Size = (Int, Int)
data TCord = TCord {uid :: Int , pos :: Cord} deriving (Show)

-- We want to still compare by position by default
instance Eq TCord where
    a == b = pos a == pos b
-- We also want to ord by pos by default
instance Ord TCord where
    a `compare` b = pos a `compare` pos b

main :: IO()
main = do
    file <- readFile "./input.txt"
    let linesOfFile = lines file
    print "starting execution"
    print $ show $ execute linesOfFile

execute xs = case (sequenceA $ map rCord xs) of
                Right a -> let listOfCords = concat a
                               overLappingCords = filter (\(a, b) -> a >= 2) . map (\a -> (length a, a)) . (group . sort) $ listOfCords
                               overLappingIds = removeDuplicates . concat . map (\(a, b) -> map uid b) $ overLappingCords
                               in uid $ head $ filter (\b -> not $ uid b `elem` overLappingIds) listOfCords
                Left _ -> -1 

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = rdHelper []
    where rdHelper seen [] = seen
          rdHelper seen (x:xs)
                  | x `elem` seen = rdHelper seen xs
                  | otherwise = rdHelper (seen ++ [x]) xs

int = read <$> many1 digit

cord :: Parser [TCord]
cord = do
    char '#'
    i <- int
    spaces
    char '@'
    spaces
    x <- int
    char ','
    y <- int
    char ':'
    spaces
    w <- int
    char 'x'
    h <- int
    pure $ cordRange i (x, y) (h, w)

rCord :: String -> Either ParseError [TCord]
rCord = parse cord "stdin"

cordRange :: Int -> Cord -> Size -> [TCord]
cordRange i (x, y) (h, w) = let rangeX = [(x + 1) .. x + w]
                                rangeY = [(y + 1) .. y + h]
                                in [TCord {uid = i, pos = (x, y)} | x <- rangeX, y <- rangeY] 
