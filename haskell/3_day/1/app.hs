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

-- TODO: Look for 2D Vector Addition ?
calcCords :: -> Cord -> Size -> [Cord]
calcCords c s = undefined

countOverlappingRow :: Row -> Int
countOverlappingRow r = length $ filter (==Overlapping) r

countOverlappingGrid :: Grid -> Int
countOverlappingGrid g = foldl (\acc r -> (+) acc $ countOverlappingRow r) 0 g 
