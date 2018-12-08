import Data.List

main :: IO ()
main = do
    file <- readFile "./input.txt"
    let linesOfFile = lines file
    putStr $ show $ execute linesOfFile

testValues = [
          "abcdef"
        , "bababc"
        , "abbcde"
        , "abcccd"
        , "aabcdd"
        , "abcdee"
        , "ababab"
        ]


execute input = go input 
            where
                go inp = let grouped = groupMatchingChilds inp
                             twoNThrees (two, three) cur = (two + (hasLengthTwoChilds cur), three + (hasLengthThreeChilds cur))
                             countTwoNThrees = foldl twoNThrees (0, 0) grouped
                          in uncurry (*) countTwoNThrees 

              
groupMatchingChilds = map ((filter (\a -> length a > 1)) . group. sort)

type Count = Int
type Length = Int

hasLengthTwoChilds :: [String] -> Int 
hasLengthTwoChilds arr = if (any (\a -> length a == 2) arr) then 1 else 0

hasLengthThreeChilds :: [String] -> Int 
hasLengthThreeChilds arr = if (any (\a -> length a == 3) arr) then 1 else 0


