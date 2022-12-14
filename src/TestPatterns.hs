module TestPatterns where

import Automata
import Extension
-- | The 1100 Pattern
pattern1100 :: Grid QRCell
pattern1100 = parseGrid toQR 11 3 cells where
  cells = concat
    [ "A A AAA AAA"
    , "A A A A A A"
    , "A A AAA AAA"
    ]

-- | The 1130 Pattern
pattern1130 :: Grid QRCell
pattern1130 = parseGrid toQR 11 5 cells where
  cells = concat
    [ "A A AAA AAA"
    , "A A   A A A"
    , "A A AAA A A"
    , "A A   A A A"
    , "A A AAA AAA"
    ]

-- | The spiral Pattern
spiral :: Grid QRCell
spiral = parseGrid toQR 34 21 cells where
  cells = concat
    [ "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
    , "                                 A"
    , "                                 A"
    , "                                 A"
    , "                                 A"
    , "                                 A"
    , "                                 A"
    , "                                 A"
    , "                                 A"
    , "                                 A"
    , "                                 A"
    , "                                 A"
    , "                                 A"
    , "                     AAAAA       A"
    , "                     A   A       A"
    , "                     A  AA       A"
    , "                     A           A"
    , "                     A           A"
    , "                     A           A"
    , "                     A           A"
    , "                     AAAAAAAAAAAAA"
    ]

-- | Given a way to parse a character, and expected bounds of the
-- grid, parse a string describing cells into a grid.
parseGrid :: (Char -> c) -> Int -> Int -> String -> Grid c
parseGrid f w h cells
  | length cells == w * h = Grid w h (map f cells)
  | otherwise = error "parseGrid: dimensions don't match"

-- Extension test patterns

simpleBattle :: Grid BattleCell
simpleBattle = parseGrid toBattleCell 11 3 cells where
  cells = concat
    [ "1 1     1  "
    , "1          "
    , "  2   2 2 2"
    ]

bigBattle :: Grid BattleCell
bigBattle = parseGrid toBattleCell 34 21 cells where
  cells = concat
    [ "                                 2"
    , "                                 2"
    , "                                 2"
    , "                                 2"
    , "                                 2"
    , "                                 2"
    , "                                 2"
    , "                                 2"
    , "                1                2"
    , "                                 2"
    , "                                 2"
    , "                                 2"
    , "                                 2"
    , "                                 2"
    , "                                 2"
    , "                                 2"
    , "                                 2"
    , "                                 2"
    , "                                 2"
    , "                                 2"
    , "                                 2"
    ]

bigBattle2 :: Grid BattleCell
bigBattle2 = parseGrid toBattleCell 34 21 cells where
  cells = concat
    [ " 1 1 1 1  1 1                    2"
    , "             1                   2"
    , "   1  11  1                      2"
    , "    1                            2"
    , "   1            22               2"
    , "  1     1       22 2             2"
    , "                22    2          2"
    , "     1          222              2"
    , "                                 2"
    , "    1   1 1             2        2"
    , "                                 2"
    , "      1  1                       2"
    , " 1       1               2       2"
    , "                     2           2"
    , "     1    111                    2"
    , "   1       1       2             2"
    , "   1     1                       2"
    , "    1  1          2    2         2"
    , "  1    1                         2"
    , "                     2           2"
    , " 1    1           2              2"
    ]

toQR :: Char -> QRCell
toQR 'A' = Alive
toQR ' ' = Dead
toQR _ = error"Not valid QR"

toBattleCell :: Char -> BattleCell
toBattleCell ' ' = Ground
toBattleCell '1' = Team1
toBattleCell '2' = Team2 
toBattleCell _ = error"Parsing error for battle grid"