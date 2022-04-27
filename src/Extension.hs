module Extension where

import CodeWorld

import Automata
   -- (Grid, GridCoord, getForNextGen, allCoords)

type Range = Int -- Range is the range of the each individual solider in each team 
type InCover = Bool

data BattleCell = Team1 Range InCover | Team2 Range InCover | Bush | Ground -- I like having new datatype in one line, looks cleaner
    deriving (Eq, Show)

renderBattleCell :: BattleCell -> Picture
renderBattleCell cell = case cell of
    Team1 _ _-> coloured red $ solidRectangle 1 1
    Team2 _ _-> coloured blue $ solidRectangle 1 1
    Bush    -> coloured (dark green) $ solidRectangle 1 1
    Ground  -> coloured (light green) $ solidRectangle 1 1

moveSolider :: Grid BattleCell -> Grid BattleCell
moveSolider = undefined

isNearCover :: Grid BattleCell -> GridCoord -> Bool
isNearCover = undefined

getDirection :: Grid BattleCell -> BattleCell -> GridCoord -> (Int, Int)
getDirection g b (x, y) = case (x3, y3) of
    (0, 0) -> (0, 0) -- Shouldn't match ever because this means the distance is 0 between two distinct objects which should never be true.
    (0, a) -> (0, a `div` abs a)
    (a, 0) -> (a `div` abs a, 0)
    (a, c) -> (a `div` abs a, c `div` abs c)
    where
        (x2, y2) = getClosestSpecificObject g b (x, y)
        (x3, y3) = (x2 - x, y2 - y)

getClosestSpecificObject :: Grid BattleCell -> BattleCell -> GridCoord -> GridCoord
getClosestSpecificObject g p c = snd $ head $ safeMin $ zip (map (distanceBetweenTwoVectors c) objectCoords) objectCoords
    where
        objectCoords = getCoords g p

        safeMin :: Ord a => [(a, b)] -> [(a, b)]
        safeMin [] = [] -- a safe version of minimum and more suited towards the data structures im using 
        safeMin list = filter ((==) (minimum (map fst list)) . fst) list

        distanceBetweenTwoVectors :: GridCoord -> GridCoord -> Double
        distanceBetweenTwoVectors (x1, y1) (x2, y2) = sqrt $ (x2d - x1d) ** 2 + (y2d - y1d) ** 2
            where
                x1d = fromIntegral x1
                x2d = fromIntegral x2
                y1d = fromIntegral y1
                y2d = fromIntegral y2

getCoords :: Grid BattleCell -> BattleCell -> [GridCoord]
getCoords (Grid a b c) object = map snd $ filter (\x -> checkEqual (fst x) object) $ zip c $ allCoords a b
    where
        checkEqual :: BattleCell -> BattleCell -> Bool
        checkEqual f s = case (f,s) of
            (Team1 _ _, Team1 _ _) -> True
            (Team2 _ _, Team2 _ _) -> True
            (Bush, Bush) -> True
            (Ground, Ground) -> True
            _ -> False

netGenBattle :: Grid BattleCell -> Grid BattleCell
netGenBattle = undefined
