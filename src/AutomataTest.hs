module AutomataTest where

import Automata
import Extension
import Testing

import TestPatterns

-- | The list of tests to run. When you define additional test cases,
-- you must list them here or they will not be checked.
tests :: [Test]
tests =
  [
    testAutomataCycle,
    testGetForNextGen,
    allCoordsTest,
    testAutomataNextState,
    getAutomataNeighoursTest1,
    getAutomataNeighoursTest2,
    testAutomataNeighbourList,
    testAutomataNextGenZip,
    testGet1,
    testGet2,
    testExtensionCycle,
    testToQR,
    testToBattle,
    allCoordsTest2,
    getCoordsTest,
    testGetNeighboursExtension,
    testGetNeighboursExtension2,
    testNextStateExtension,
    testNextStateExtension2,
    testNextStateExtension3,
    getDistanceTesting,
    getClosestObjectTest,
    getClosestObjectTest2,
    safeMinTest,
    getNextPointTest,
    validMovementTest,
    getNextPointTest2
  ]

-- | Example test case. The String argument to 'Test' is a label that
-- identifies the test, and gives the reader some idea about what's
-- being tested. For simple arithmetic, these should be obvious, but
-- when you write tests for your code, you can use this space to say
-- things like "the next state for a cell with 3 live neighbours is
-- 'Alive'".
--

-- (Task 1 test used in both Task 2 & 3)

testGet1 :: Test
testGet1 = Test "get (Grid 2 2 [Team1, Ground, Ground, Team2]) (1,1) == Team2" $ assertEqual (get (Grid 2 2 [Team1, Ground, Ground, Team2]) (1,1)) $ Just Team2

testGet2 :: Test
testGet2 = Test "get (Grid 2 2 [Alive, Dead, Alive, Alive]) (-1,2) == Nothing" $ assertEqual (get (Grid 2 2 [Alive, Dead, Alive, Alive]) (-1,2) ) Nothing

testAutomataCycle :: Test
testAutomataCycle = Test "cycleQR Dead == Alive" $ assertEqual (cycleQR Dead) Alive

testExtensionCycle :: Test
testExtensionCycle = Test "cycleBattle Team1 == Team2" $ assertEqual (cycleBattle Team1) Team2

allCoordsTest :: Test
allCoordsTest = Test "allCoords 3 3 == [(0,0),(1,0),(2,0),(0,1),(1,1),(2,1),(0,2),(1,2),(2,2)]" $ assertEqual (allCoords 3 3) [(0,0),(1,0),(2,0),(0,1),(1,1),(2,1),(0,2),(1,2),(2,2)]

allCoordsTest2 :: Test
allCoordsTest2 = Test "allCoords (-1) 3 == []" $ assertEqual (allCoords (-1) 3) []

testToQR :: Test
testToQR = Test "toQR ' ' == Dead" $ assertEqual (toQR ' ') Dead

testToBattle :: Test
testToBattle = Test "toBattleCell '1' == Team1" $ assertEqual (toBattleCell '1') Team1

-- Task 2 Tests (QR world)

testGetForNextGen :: Test
testGetForNextGen = Test "getForNextGen (Grid 2 2 [Alive, Alive, Alive, Dead]) (1,0) == Alive" $ assertEqual (getForNextGen (Grid 2 2 [Alive, Alive, Alive, Dead]) (1,0)) Alive

testAutomataNextState :: Test
testAutomataNextState = Test "Automata.nextState (Alive, [Alive, Dead, Dead, Alive]) == Alive" $ assertEqual (Automata.nextState (Alive, [Alive, Dead, Dead, Alive])) Alive

getAutomataNeighoursTest1 :: Test
getAutomataNeighoursTest1 = Test "Automata.getNeighboursCoords (Grid 2 2 [Alive, Dead, Dead, Alive]) (1,0) == [(0,0), (1,1)]" $ assertEqual expression answer
  where
    expression = Automata.getNeighboursCoords (Grid 2 2 [Alive, Dead, Dead, Alive]) (1,0)
    answer = [(0,0), (1,1)]

getAutomataNeighoursTest2 :: Test
getAutomataNeighoursTest2 = Test "Automata.getNeighboursCoords (Grid 3 3) [Alive, Dead, Dead, Alive, Alive, Dead, Dead, Alive, Alive] (2,1) == [(2,0), (1,1), (2,2)]" (assertEqual expression answer)
  where
    expression = Automata.getNeighboursCoords (Grid 3 3 [Alive, Dead, Dead, Alive, Alive, Dead, Dead, Alive, Alive]) (2,1)
    answer = [(1,1), (2,0), (2,2)]

testAutomataNeighbourList :: Test
testAutomataNeighbourList = Test "[map (getForNextGen (Grid 2 2 [Alive, Alive, Dead, Alive])) points | points <- map (getNeighboursCoords (Grid 2 2 [Alive, Alive, Dead, Alive])) (allCoords 2 2)] == [(Alive, Dead), (Alive, Alive), (Alive, Alive), (Dead, Alive)]" (assertEqual neighbourList answer)
  where
    neighbourList = [map (getForNextGen (Grid 2 2 [Alive, Alive, Dead, Alive])) points | points <- map (Automata.getNeighboursCoords (Grid 2 2 [Alive, Alive, Dead, Alive])) (allCoords 2 2)]
    answer = [[Alive, Dead], [Alive, Alive], [Alive, Alive], [Dead, Alive]]

testAutomataNextGenZip :: Test
testAutomataNextGenZip = Test " zip (map (getForNextGen (Grid 2 2 [Alive, Alive, Dead, Alive])) (allCoords 2 2)) neighbourList == [(Alive, [Alive, Dead]), (Alive, [Alive, Alive]), (Dead, [Alive, Alive]), (Alive, [Dead, Alive])]" $ assertEqual expression answer
  where
    neighbourList = [map (getForNextGen (Grid 2 2 [Alive, Alive, Dead, Alive])) points | points <- map (Automata.getNeighboursCoords (Grid 2 2 [Alive, Alive, Dead, Alive])) (allCoords 2 2)]
    expression = zip (map (getForNextGen (Grid 2 2 [Alive, Alive, Dead, Alive])) (allCoords 2 2)) neighbourList
    answer = [(Alive, [Alive, Dead]), (Alive, [Alive, Alive]), (Dead, [Alive, Alive]), (Alive, [Dead, Alive])]

-- Task 3 (Extension tests)

-- Helper functions

getDistanceTesting :: Test
getDistanceTesting = Test "sqrt $ (2 - 1) ** 2 + (2 - 1) ** 2 == 1" $ assertApproxEqual (sqrt $ (2 - 1) ** 2 + (2 - 1) ** 2) 1.4142135623730951 -- Used in calculating the distance between enemies (or other objects)

getCoordsTest :: Test
getCoordsTest = Test "getCoords (Grid 3 3 [Team1, Ground, Ground, Ground, Team2, Team2, Team2, Team1, Team2]) Team1 == [(0,0), (1, 2)]" $ assertEqual (getCoords (Grid 3 3 [Team1, Ground, Ground, Ground, Team2, Team2, Team2, Team1, Team2]) Team1) [(0,0), (1, 2)]

getClosestObjectTest :: Test
getClosestObjectTest = Test "getClosestSpecificObject (Grid 3 3 [Team1, Ground, Ground, Ground, Ground, Ground, Ground, Ground, Team2]) Team2 (0,0) == Just (2,2)" $ assertEqual expression answer
  where
    expression = getClosestSpecificObject (Grid 3 3 [Team1, Ground, Ground, Ground, Ground, Ground, Ground, Ground, Team2]) Team2 (0,0)
    answer = Just (2,2)

getClosestObjectTest2 :: Test
getClosestObjectTest2 = Test "getClosestSpecificObject (Grid 3 3 [Team1, Ground, Ground, Ground, Ground, Ground, Ground, Ground, Ground]) Team2 (0,0) == Just (2,2)" $ assertEqual expression answer
  where
    expression = getClosestSpecificObject (Grid 3 3 [Team1, Ground, Ground, Ground, Ground, Ground, Ground, Ground, Ground]) Team2 (0,0)
    answer = Nothing

testGetNeighboursExtension :: Test
testGetNeighboursExtension = Test "Extension.getNeighboursCoords (Grid 3 3 [Team1, Ground, Team1, Ground, Ground, Team2, Team2, Team2, Ground]) (1,1) == [(0,1),(2,1),(1,0),(1,2),(2,2),(0,2),(2,0),(0,0)]" $ assertEqual expression answer
  where
    expression = Extension.getNeighboursCoords (Grid 3 3 [Team1, Ground, Team1, Ground, Ground, Team2, Team2, Team2, Ground]) (1,1)
    answer = [(0,1),(2,1),(1,0),(1,2),(2,2),(0,2),(2,0),(0,0)]

testGetNeighboursExtension2 :: Test
testGetNeighboursExtension2 = Test "Extension.getNeighboursCoords (Grid 2 2 [Team1, Ground, Ground, Team2]) (0,0) == [(1,0), (0,1), (1,1)]" $ assertEqual expression answer
  where
    expression = Extension.getNeighboursCoords (Grid 2 2 [Team1, Ground, Ground, Team2]) (0,0)
    answer = [(1,0), (0,1), (1,1)]


safeMinTest :: Test
safeMinTest = Test "safeMin [(1.0, 2.0), (3.0, 2.0)] == [(1.0,2.0)]" $ assertEqual (safeMin [(1.0 :: Double, 2.0 :: Double), (3.0, 2.0)]) [(1.0 :: Double,2.0)]
  where
        safeMin :: Ord a => [(a, b)] -> [(a, b)]
        safeMin [] = []
        safeMin list = filter ((==) (minimum (map fst list)) . fst) list

-- Attack phase of the battle testing 

testNextStateExtension :: Test
testNextStateExtension = Test "Extension.nextState (Team1, [Team2, Ground, Team2]) == Ground" $ assertEqual (Extension.nextState (Team1, [Team2, Ground, Team2])) Ground

testNextStateExtension2 :: Test
testNextStateExtension2 = Test "Extension.nextState (Team2, [Team2, Ground, Team2]) == Team2" $ assertEqual (Extension.nextState (Team2, [Team2, Ground, Team2])) Team2

testNextStateExtension3 :: Test
testNextStateExtension3 = Test "Extension.nextState (Ground, []) == Ground " $ assertEqual (Extension.nextState (Ground, [])) Ground

-- Moving the soldiers testing

getNextPointTest :: Test
getNextPointTest = Test "getNextPoint (0,0) (Just (2,2)) == (1,1)" $ assertEqual (getNextPoint (0,0) (Just (2,2))) (1,1)

getNextPointTest2 :: Test
getNextPointTest2 = Test "getNextPoint (0,0) Nothing == (0,0)" $ assertEqual (getNextPoint (0,0) Nothing) (0,0)

validMovementTest :: Test
validMovementTest = Test "validMovement (Grid 3 3 [Team1, Ground, Ground, Ground, Team1, Ground, Ground, Ground, Team2]) (1,1) == False" $ assertEqual expression False
  where
    expression = validMovement (Grid 3 3 [Team1, Ground, Ground, Ground, Team1, Ground, Ground, Ground, Team2]) (1,1)

-- nextMovementPhaseTest :: Test, Although this test won't run, because Grid BattleCell is equatable. If we run this expression in the terminal, the output is the expected outcome / answer given
-- nextMovementPhaseTest = Test "nextMovementPhase (Grid 3 3 [Team1, Team1, Ground, Ground, Ground, Ground, Team2, Team2, Team2]) (allCoords 3 3) == Grid 3 3 [Ground,Ground,Ground,Team1,Team1,Ground,Team2,Team2,Team2]" $ assertEqual expression answer
  -- where
    -- expression = nextMovementPhase (Grid 3 3 [Team1, Team1, Ground, Ground, Ground, Ground, Team2, Team2, Team2]) $ allCoords 3 3
    -- answer = Grid 3 3 [Ground, Ground, Ground, Team1, Team1, Ground, Team2, Team2, Team2] 
    -- the top Team1 cell (0,0) matches with Team2 (0,2) and moves in the direction (0,0) + (0,1) whilst Team1 (1,0) Moves towards Team2 (1,2) in the direction (1,0) + (0,1)

-- moveSolidersTest :: Test, Move soliders is simply an application of it's helper functions nextMovement phase, thus it is guarenteed that given nextMovement functions properly, this function
-- will also work properly. 
