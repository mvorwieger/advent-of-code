import Data.Maybe

type State = (Int, [Int])
    
main :: IO()
main = do
    file <- readFile "./input.txt"
    let linesOfFile = lines file
    print $ calc (linesOfFile) (0, [])

calc :: [String] -> (Int, [Int]) -> Int
calc arr preState = let state = foldl (flip nextStep) preState arr 
                        past  = snd state
                        in case repeated (reverse past) of
                            Just a  -> a
                            Nothing -> calc arr state

nextStep :: String -> State -> State
nextStep ('-':number) (currentState, pastStates) = let newState = currentState - (read $ number :: Int)
                                                    in (newState, newState : pastStates)
nextStep ('+':number) (currentState, pastStates) = let newState = currentState + (read $ number :: Int)
                                                    in (newState, newState : pastStates)
nextStep _ state = state

repeated :: [Int] -> Maybe Int
repeated xs = go xs []
    where 
        go [] _ = Nothing
        go (x : xs) visited =
            if x `elem` visited
                then Just x
                else go xs (x : visited)
