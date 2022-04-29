module Extension where

import CodeWorld

import Automata ( allCoords, getForNextGen, Grid(..), GridCoord)

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

data BattleCell = Team1 | Team2 | Ground -- I like having new datatype in one line, looks cleaner
    deriving (Eq, Show)

cycleBattle :: BattleCell -> BattleCell
cycleBattle cell = case cell of
    Team1 -> Team2
    Team2 -> Ground
    Ground -> Team1

renderBattleCell :: BattleCell -> Picture
renderBattleCell cell = case cell of
    Team1-> coloured red $ solidRectangle 1 1
    Team2-> coloured blue $ solidRectangle 1 1
    Ground  -> coloured (light green) $ solidRectangle 1 1

getNeighboursCoords :: Grid c -> GridCoord -> [GridCoord]
getNeighboursCoords (Grid a b _) (x, y)
    | x < xBound && x > 0 &&  y < yBound && y > 0 = [(x - 1, y), (x + 1, y), (x, y - 1),  (x, y + 1), (x + 1, y + 1), (x - 1, y + 1), (x + 1, y - 1), (x - 1, y - 1)] -- general case when x or y aren't on the bounds
    | x == xBound && y < yBound && y > 0 = [(x - 1, y), (x, y - 1), (x, y + 1), (x - 1, y - 1), (x - 1, y + 1)] -- at the right x bound (But not corners)
    | x == 0 &&  y < yBound && y > 0 = [(x + 1, y), (x, y - 1), (x, y + 1), (x + 1, y - 1), (x + 1, y + 1)] -- at the left x bound
    | x < xBound && x > 0 &&  y == yBound = [(x - 1, y), (x + 1, y), (x, y - 1), (x + 1, y - 1), (x - 1, y - 1)] -- at the bottom y bound
    | x < xBound && x > 0 && y == 0 = [(x - 1, y), (x + 1, y), (x, y + 1), (x + 1, y + 1), (x - 1, y + 1)] -- at the top y bound
    | x == 0 && y == 0 = [(x + 1, y), (x, y + 1), (x + 1, y + 1)] -- top left
    | x == xBound && y == 0 = [(x - 1, y), (x, y + 1), (x - 1, y + 1)] -- top right
    | x == 0 && y == yBound = [(x + 1, y), (x, y - 1), (x + 1, y - 1)] -- bottom left
    | x == xBound && y == yBound = [(x - 1, y), (x, y - 1), (x - 1, y - 1)]-- bottom right
    | otherwise = error "Out of bounds"
        where
            xBound = a - 1
            yBound = b - 1

getCoords :: Grid BattleCell -> BattleCell -> [GridCoord]
getCoords (Grid a b c) object = map snd $ filter (\x -> fst x ==  object) $ zip c $ allCoords a b

nextGenBattle :: Grid BattleCell -> Grid BattleCell
nextGenBattle (Grid a b c) = Grid a b $ zipWith (curry nextState) (getList (moveSoliders (Grid a b c))) neighbourList
    where
        neighbourList = [map (getForNextGen (Grid a b c)) points | points <- map (getNeighboursCoords (Grid a b c)) (allCoords a b)]

getList :: Grid BattleCell -> [BattleCell]
getList (Grid _ _ c) = c

evolveBattle :: Int -> Grid BattleCell -> Grid BattleCell
evolveBattle n g = iterate nextGenBattle g !! n


-- Move phase of the battle
moveSoliders :: Grid BattleCell -> Grid BattleCell
moveSoliders (Grid a b c) = nextMovementPhase (Grid a b c) $ allCoords a b

nextMovementPhase :: Grid BattleCell -> [GridCoord] -> Grid BattleCell
nextMovementPhase g [] = g
nextMovementPhase (Grid a b c) (y:ys) = case cell of
    Team1 | validMovement g nextPoint1 -> nextMovementPhase (setAt y Ground (setAt nextPoint1 Team1 g)) $ removeModifiedPoint nextPoint1 ys
    Team2 | validMovement g nextPoint2 -> nextMovementPhase (setAt y Ground (setAt nextPoint2 Team2 g)) $ removeModifiedPoint nextPoint2 ys
    _ -> nextMovementPhase (setAt y cell g) ys
    where
        cell = getForNextGen g y
        g = Grid a b c
        nextPoint1 = getNextPoint y (getClosestSpecificObject g Team2 y)
        nextPoint2 = getNextPoint y (getClosestSpecificObject g Team1 y)

        removeModifiedPoint :: Eq a => a -> [a] -> [a]
        removeModifiedPoint q xs = [x | x <- xs, x /= q]

validMovement :: Grid BattleCell -> GridCoord -> Bool
validMovement g a = case battleType of
    Ground -> True
    Team1 -> False
    Team2 -> False
    where
        battleType = getForNextGen g a

getNextPoint :: GridCoord -> Maybe GridCoord -> GridCoord
getNextPoint a Nothing = a -- When there are no enemies
getNextPoint (x, y) (Just (x2, y2)) = case (x3, y3) of
    (0, 0) -> (0, 0) -- should never match
    (0, a) -> (x, y + (a `div` abs a)) -- moving vertically
    (a, 0) -> (x + (a `div` abs a), y) -- moving horizontally
    (a, c) -> (x + (a `div` abs a), y + (c `div` abs c)) -- moving diagnonally
    where
        (x3, y3) = (x2 - x, y2 - y)

getClosestSpecificObject :: Grid BattleCell -> BattleCell -> GridCoord -> Maybe GridCoord
getClosestSpecificObject g p c = (safeSnd . safeHead . safeMin) $ zip (map (distanceBetweenTwoVectors c) (getCoords g p)) $ getCoords g p
    where
        safeSnd :: Maybe (a, b) -> Maybe b
        safeSnd Nothing = Nothing
        safeSnd (Just q) = Just $ snd q

        safeHead :: [a] -> Maybe a
        safeHead [] = Nothing
        safeHead a = Just $ head a

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

-- Attack phase of the battle

nextState :: (BattleCell, [BattleCell]) -> BattleCell
nextState (Team1, li)
    | enemies > 1 && allies <= enemies = Ground
    | otherwise = Team1
    where
        enemies = length $ filter (== Team2) li
        allies = length $ filter (== Team1) li

nextState (Team2, li)
    | enemies2 > 0 && allies2 <= enemies2 = Ground
    | otherwise = Team2
    where
        enemies2 = length $ filter (== Team1) li
        allies2 = length $ filter (== Team2) li

nextState(cell, _) = cell -- Ground
