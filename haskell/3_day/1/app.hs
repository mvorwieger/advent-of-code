data Field = Empty | Cell Int | Overlapping deriving (Show, Eq)

type Row = [Field]
type Grid = [Row]
