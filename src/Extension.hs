module Extension where

import CodeWorld

import Automata 
    (Grid, GridCoord, getForNextGen, allCoords)

type Range = Int -- Range is the range of the each individual solider in each team 
type InCover = Bool

data BattleCell = Team1 Range InCover | Team2 Range InCover | Bush | Ground -- I like having new datatype in one line, looks cleaner
    deriving (Eq, Show)

renderBattleCell :: BattleCell -> Picture
renderBattleCell cell = case cell of
    Team1 _ _-> coloured red $ rectangle 1 1
    Team2 _ _-> coloured blue $ rectangle 1 1
    Bush    -> coloured (dark green) $ rectangle 1 1
    Ground  -> coloured (light green) $ rectangle 1 1

moveSolider :: Grid BattleCell -> Grid BattleCell
moveSolider = undefined

getClosestEnemy :: Grid BattleCell -> GridCoord -> GridCoord 
getClosestEnemy = undefined

getClosestEnemyInRange :: Grid BattleCell -> GridCoord -> 

getEnemyDirection :: GridCoord -> GridCoord -> (Int, Int)
getEnemyDirection = undefined

isNearCover :: Grid BattleCell -> GridCoord -> BattleCell -- returns a modified solider cell
isNearCover = undefined

getClosestBush :: [GridCoord] -> GridCoord -> BattleCell -- returns a bush
getClosestBush a b = min $ map (distanceBetweenTwoVectors b) a

distanceBetweenTwoVectors :: GridCoord -> GridCoord -> Double
distanceBetweenTwoVectors (x1, y1) (x2, y2) = sqrt $ (x2 - x1) ** 2 + (y2 - y1) ** 2

getCoords :: Grid BattleCell -> BattleCell -> [GridCoord]
getCoords (Grid a b c) object = map snd $ filter (\x -> checkEqual (fst x) object) $ zip c $ allCoords a b
    where
        checkEqual :: BattleCell -> BattleCell -> Bool
        checkEqual a b = case (a,b) of
            (Team1 _ _, Team1 _ _) -> True
            (Team2 _ _, Team2 _ _) -> True
            (Bush, Bush) -> True
            (Ground, Ground) -> True
            _ -> False

netGenBattle :: Grid BattleCell -> Grid BattleCell
netGenBattle = undefined

