import Data.List
import Control.Arrow 
import Data.Char
import Text.Parsec
import Text.Parsec.Char (spaces, digit, char)
import Text.Parsec.String (Parser)
import Text.Parsec (parse, many1, ParseError)

type Cord = (Int, Int)
type Size = (Int, Int)

main :: IO()
main = do
    file <- readFile "./input.txt"
    let linesOfFile = lines file
    print "starting execution"
    print $ show $ execute linesOfFile

execute xs = case (sequenceA $ map rCord xs) of
                Right a -> length . filter (\a -> a >= 2) . map length . (group . sort) $ concat a 
                Left _ -> -1

int = read <$> many1 digit

cord :: Parser [Cord]
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
    pure $ cordRange (x, y) (h, w)

rCord :: String -> Either ParseError [Cord]
rCord = parse cord "stdin"

cordRange :: Cord -> Size -> [Cord] 
cordRange (x, y) (h, w) = let rangeX = [(x + 1) .. x + w]
                              rangeY = [(y + 1) .. y + h]
                              in [(x, y) | x <- rangeX, y <- rangeY] 
