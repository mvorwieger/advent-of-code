main :: IO()
main = do
    file <- readFile "./input.txt"
    let linesOfFile = lines file
    putStr $ show $ calc linesOfFile

calc :: [String] -> Int
calc arr = foldl (flip nextStep)  0 arr

nextStep :: String -> Int -> Int
nextStep str state 
    | head str == '-' = state - (read $ tail str :: Int)
    | head str == '+' = state + (read $ tail str :: Int)
