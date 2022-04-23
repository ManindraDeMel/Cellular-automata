module Automata where

import CodeWorld
-- | A 'Grid' of some cell type c consists of a pair of positive ints
-- describing its width and height, along with a list of cs that is
-- exactly (width * height) long.
data Grid c = Grid Int Int [c]

-- | Type alias for grid coordinates. This makes it clear when we are
-- talking about grid coordinates specifically, as opposed to some
-- other pair of integers.
type GridCoord = (Int, Int)


-- | Type of cells used in QR World.
data QRCell = Alive | Dead
    deriving (Eq, Show)

cycleQR :: QRCell -> QRCell
cycleQR Alive = Dead
cycleQR Dead = Alive

renderQR :: QRCell -> Picture
renderQR Alive = rectangle 1 1
renderQR Dead = coloured blue $ solidRectangle 1 1

nextGenQR :: Grid QRCell -> Grid QRCell
nextGenQR (Grid a b c) = (map . map) get (map (getAliveNeighboursCoords (Grid a b c)) (allCoords a b))

nextState :: QRCell -> [QRCell] -> QRCell
nextState Alive neighbourStates
    | length (filter (== Dead) neighbourStates) < 2 = Dead -- less than two neighbours then die
    | length (filter (== Dead) neighbourStates) == 4 = Dead -- Overcrowded, die again
    | otherwise = Alive
nextState Dead neighbourStates
    | length (filter (== Alive) neighbourStates) == 2 || length (filter (== Alive) neighbourStates) == 4 = Alive -- Alive due to breeding
    | otherwise = Dead

getAliveNeighboursCoords :: Grid c -> GridCoord -> [GridCoord]
getAliveNeighboursCoords (Grid a b _) (x, y)
    | x < a && x > 0 &&  y > b && y > 0 = [(x, y), (x - 1, y), (x + 1, y), (x, y - 1),  (x, y + 1)] -- general case when x or y aren't on the bounds
    | x == a && y > b && y > 0 = [(x, y), (x - 1, y), (x, y - 1), (x, y + 1)] -- at the right x bound (But not corners)
    | x == 0 &&  y > b && y > 0 = [(x, y), (x + 1, y), (x, y - 1), (x, y + 1)] -- at the left x bound
    | x < a && x > 0 &&  y == b = [(x, y), (x - 1, y), (x + 1, y), (x, y - 1)] -- at the bottom y bound
    | x < a && x > 0 && y == 0 = [(x, y), (x - 1, y), (x + 1, y), (x, y + 1)] -- at the top y bound
    | x == 0 && y == 0 = [(x, y), (x + 1, y), (x, y + 1)] -- top left
    | x == a && y == 0 = [(x, y), (x - 1, y), (x, y + 1)] -- top right
    | x == 0 && y == b = [(x, y), (x + 1, y), (x, y - 1)] -- bottom left
    | x == a && y == b = [(x, y), (x - 1, y), (x, y - 1)]-- bottom right
    | otherwise = error "Out of bounds"

evolveQR :: Int -> Grid QRCell -> Grid QRCell
evolveQR = undefined -- TODO


get :: Grid c -> GridCoord -> Maybe c
get (Grid a b c) (x, y)
    | x <= a && y <= b = Just $ c !! (y * a + x)
    | otherwise = Nothing

allCoords :: Int -> Int -> [GridCoord]
allCoords a b = [(x, y) | y <- [0..b-1], x <- [0..a-1]]
