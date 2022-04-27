{-# LANGUAGE OverloadedStrings #-}

module App where
import Extension
import Automata
import CodeWorld
import Data.List (splitAt)
import Data.Text (pack)
import GridRenderer
import TestPatterns
import GridRenderer (renderGrid)

appMain :: IO ()
appMain = activityOf (Model 0 1 (QR pattern1100)) handleEvent render

-- | The first 'Int' is the number of generations evolved. The second
-- is how far to jump when pressing space.
data Model = Model Int Int CellGrid

-- | The model at the start of a simulation.
initial :: CellGrid -> Model
initial = Model 0 1

data CellGrid
  = QR (Grid QRCell)
  | Battle (Grid BattleCell)

-- | Events we'd like our program to handle. Returned by 'parseEvent'.
data AppEvent
  = ChangeCell GridCoord
    -- ^ Change a cell in the grid, in a way that makes sense for the
    -- current automaton.
  | LoadTestPattern TestPattern
    -- ^ Replace the grid with one of the test patterns.
  | ToQR
    -- ^ Switch to QR World.
  | ToBattle
    -- ^ Switch to Battle World
  | Step
    -- ^ Run the automaton one step.
  | Jump
    -- ^ Jump forward a few steps.
  | IncreaseJumpSize

  | DecreaseJumpSize

data TestPattern = One | Two | Three

handleEvent :: Event -> Model -> Model
handleEvent ev m = case parseEvent m ev of
  Nothing -> m
  Just appEv -> applyEvent appEv m

-- | CodeWorld has many events and we are not interested in most of
-- them. Parse them to our app-specific event type.
--
-- Further reading, if interested: "Parse, don't validate"
-- https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/
parseEvent :: Model -> Event -> Maybe AppEvent
parseEvent (Model _ _ grid) ev = case ev of
  KeyPress k
    | k == "1" -> Just (LoadTestPattern One)
    | k == "2" -> Just (LoadTestPattern Two)
    | k == "3" -> Just (LoadTestPattern Three)
    | k == "Q" -> Just ToQR
    | k == "B" -> Just ToBattle
    | k == "." -> Just Step
    | k == " " -> Just Jump
    | k == "=" -> Just IncreaseJumpSize
    | k == "-" -> Just DecreaseJumpSize
    | otherwise -> Nothing
  PointerPress p -> case getGridCoord p of
    Nothing -> Nothing
    Just coord -> Just (ChangeCell coord)
  _ -> Nothing

  where
    getGridCoord p = case grid of
      QR g -> fromPoint g p
      Battle g -> fromPoint g p

applyEvent :: AppEvent -> Model -> Model
applyEvent ev (Model n steps grid) = case ev of
  ToQR -> initial (QR pattern1100)
  ToBattle -> initial (Battle simpleBattle)
  ChangeCell p -> Model n steps grid'
    where
      grid' = case grid of
        QR cells -> QR (at p cycleQR cells)
  LoadTestPattern pat -> initial grid'
    where
      grid' = case (pat, grid) of
        (One, QR _)   -> QR pattern1100
        (Two, QR _)   -> QR pattern1130
        (Three, QR _) -> QR spiral
  Step -> Model (n + 1) steps grid'
    where
      grid' = case grid of
        QR g -> QR (nextGenQR g)
  Jump -> Model (n + steps) steps grid'
    where
      grid' = case grid of
        QR g -> QR (evolveQR steps g)
  IncreaseJumpSize -> Model n (steps + 1) grid
  DecreaseJumpSize -> Model n (max 1 (steps - 1)) grid

render :: Model -> Picture
render (Model n steps grid)
  = translated (-10) 9 (lettering (pack ("Generation: " ++ show n)))
  & translated (-10) 8 (lettering (pack ("Step size: " ++ show steps)))
  & case grid of
      QR g -> renderGrid renderQR g
      Battle g -> renderGrid renderBattleCell g

-- | Apply a function to a certain cell inside a grid, and return a
-- new grid where that cell has been replaced with the result of the
-- function call.
at :: GridCoord -> (c -> c) -> Grid c -> Grid c
at p@(x, y) f g@(Grid w h cells) = case get g p of
  Nothing -> g
  Just c -> Grid w h cells' where
    cells' = beforeCells ++ f c:afterCells
    (beforeCells, _:afterCells) = splitAt (w * y + x) cells

-- | Replace a cell within a grid.
setAt :: GridCoord -> c -> Grid c -> Grid c
setAt p c = at p (const c)
