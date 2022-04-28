module Extension where

import CodeWorld

import Automata ( allCoords, getForNextGen, Grid(..), GridCoord )

type InCover = Bool

get :: Grid c -> GridCoord -> Maybe c
get (Grid a b c) (x, y)
    | x <= a && y <= b && x >= 0 && y >= 0 = Just $ c !! (y * a + x)
    | otherwise = Nothing

at :: GridCoord -> (c -> c) -> Grid c -> Grid c
at p@(x, y) f g@(Grid w h cells) = case get g p of
  Nothing -> g
  Just c -> Grid w h cells' where
    cells' = beforeCells ++ f c:afterCells
    (beforeCells, _:afterCells) = splitAt (w * y + x) cells

setAt :: GridCoord -> c -> Grid c -> Grid c
setAt p c = at p (const c)

data BattleCell = Team1 InCover | Team2 InCover | Bush | Ground -- I like having new datatype in one line, looks cleaner
    deriving (Eq, Show)

cycleBattle :: BattleCell -> BattleCell
cycleBattle cell = case cell of
    Team1 b -> Team2 b
    Team2 _ -> Bush
    Bush -> Ground
    Ground -> Team1 False

renderBattleCell :: BattleCell -> Picture
renderBattleCell cell = case cell of
    Team1 _-> coloured red $ solidRectangle 1 1
    Team2 _-> coloured blue $ solidRectangle 1 1
    Bush    -> coloured (dark green) $ solidRectangle 1 1
    Ground  -> coloured (light green) $ solidRectangle 1 1

getCoords :: Grid BattleCell -> BattleCell -> [GridCoord]
getCoords (Grid a b c) object = map snd $ filter (\x -> checkEqual (fst x) object) $ zip c $ allCoords a b
    where
        checkEqual :: BattleCell -> BattleCell -> Bool
        checkEqual f s = case (f,s) of
            (Team1 _, Team1 _) -> True
            (Team2 _, Team2 _) -> True
            (Bush, Bush) -> True
            (Ground, Ground) -> True
            _ -> False

netGenBattle :: Grid BattleCell -> Grid BattleCell
netGenBattle = undefined

evolveBattle :: Int -> Grid BattleCell -> Grid BattleCell
evolveBattle = undefined


-- Move phase of the battle
moveSoliders :: Grid BattleCell -> Grid BattleCell
moveSoliders (Grid a b c) = nextMovementPhase (Grid a b c) $ allCoords a b

nextMovementPhase :: Grid BattleCell -> [GridCoord] -> Grid BattleCell
nextMovementPhase g [] = g
nextMovementPhase g (y:ys) = case cell of
    Team1 x | validMovement g nextPoint1 -> nextMovementPhase (setAt y Ground (setAt nextPoint1 (Team1 x) g)) ys
    Team2 x | validMovement g nextPoint2 -> nextMovementPhase (setAt y Ground (setAt nextPoint2 (Team2 x) g)) ys
    _ -> nextMovementPhase (setAt (getDirection g cell y) cell g) ys
    where
        cell = getForNextGen g y
        nextPoint1 = getDirection g (Team2 False) y
        nextPoint2 = getDirection g (Team1 False) y

validMovement :: Grid BattleCell -> GridCoord -> Bool
validMovement g a = case battleType of
    Ground -> True
    Bush -> False
    Team1 _ -> False
    Team2 _ -> False
    where
        battleType = getForNextGen g a

getDirection :: Grid BattleCell -> BattleCell -> GridCoord -> GridCoord
getDirection g b (x, y) = case (x3, y3) of
    (0, 0) -> (0, 0) -- Shouldn't match ever because this means the distance is 0 between two distinct objects which should never be true.
    (0, a) -> (0, y + (a `div` abs a))
    (a, 0) -> (x + (a `div` abs a), 0)
    (a, c) -> (x + (a `div` abs a), y + (c `div` abs c))
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

-- Check for cover phase of the battle

isNearCover :: Grid BattleCell -> GridCoord -> Bool
isNearCover = undefined



-- Attack phase of the battle