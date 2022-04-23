module AutomataTest where

--import Automata
import Testing

-- | The list of tests to run. When you define additional test cases,
-- you must list them here or they will not be checked. You should
-- remove these examples when submitting the assignment.
tests :: [Test]
tests =
  [ exampleTest
  , exampleFailure
  ]

-- | Example test case. The String argument to 'Test' is a label that
-- identifies the test, and gives the reader some idea about what's
-- being tested. For simple arithmetic, these should be obvious, but
-- when you write tests for your code, you can use this space to say
-- things like "the next state for a cell with 3 live neighbours is
-- 'Alive'".
--
-- You should remove this test before submitting your assignment.
exampleTest :: Test
exampleTest = Test "2 + 2 == 4" (assertEqual (2 + 2) (4 :: Int))

-- | This test will fail, so you can see what a failing test looks
-- like.
--
-- You should remove this test before submitting your assignment.
exampleFailure :: Test
exampleFailure = Test "0.1 + 0.2 == 0.3" (assertEqual (0.1 + 0.2) (0.3 :: Double))
