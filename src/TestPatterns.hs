module TestPatterns where

import Automata

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

toQR :: Char -> QRCell
toQR 'A' = Alive
toQR ' ' = Dead
toQR _ = error"Not valid QR"