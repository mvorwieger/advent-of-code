import Data.List

data Field = Empty | Cell Int | Overlapping deriving (Show, Eq)
type Row = [Field]
type Grid = [Row]
type Cord = (Int, Int)
type Size = (Int, Int)
type Id = Int

exampleGrid = [
      [Empty, Cell 5, Overlapping]
    , [Cell 2, Cell 5, Overlapping]
    , [Empty, Empty, Overlapping]
    ]

insert :: Id -> Cord -> Size -> Grid -> Grid
insert i c s = undefined

-- replicate :: Int -> a -> [a]
-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
-- TODO: Look for 2D Vector Addition ?
-- calcCords :: Cord -> Size -> [Field]
calcCords (x, y) (h, w) = let 
        gridWidth    = x + w
        gridHeight  = y + h
        body        = replicate (gridHeight) $ replicate (gridWidth) Empty 
        rangeX      = (x, x + w)
        rangeY      = (y, y + h)
            in body 

countOverlappingRow :: Row -> Int
countOverlappingRow r = length $ filter (==Overlapping) r

countOverlappingGrid :: Grid -> Int
countOverlappingGrid g = foldl (\acc r -> (+) acc $ countOverlappingRow r) 0 g 
