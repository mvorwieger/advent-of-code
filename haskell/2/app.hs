import Data.Maybe

main :: IO()
main = do
    file <- readFile "./input.txt"
    let linesOfFile = lines file
    print $ calc (linesOfFile) (0, [])

calc :: [String] -> (Int, [Int]) -> Int
calc arr preState = let 
    state = foldl (flip nextStep) preState arr 
    past = snd state
    duplicate = repeated past
    in if isJust duplicate 
        then fromJust duplicate
        else calc arr state 

type State = (Int, [Int])

nextStep :: String -> State -> State
nextStep str state 
    | head str == '-' = let newState = (fst state) - (read $ tail str :: Int)
                            pastStates = snd state
                            in (newState, pastStates ++ [newState])
    | head str == '+' = let newState = (fst state) + (read $ tail str :: Int)
                            pastStates = snd state
                            in (newState, pastStates ++ [newState])

repeated :: [Int] -> Maybe Int
repeated xs = go xs []
    where 
        go [] _ = Nothing
        go (x : xs) visited =
            if x `elem` visited
                then Just x
                else go xs (x : visited)
