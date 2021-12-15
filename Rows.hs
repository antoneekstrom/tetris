module Rows where

import Data.Maybe (isJust)
import Utilities (Color (..), Position)

-- | Value of a cell.
type Cell = Maybe Color

-- | Row of cells.
type Row = [Cell]

-- | Clears any full rows and returns the score gained.
clear :: [Row] -> ([Row], Int)
clear rows = (replicate cleared (replicate rowLen Nothing) ++ remaining, cleared)
  where
    remaining = filter (not . all isJust) rows
    cleared = length rows - length remaining
    rowLen = length $ head rows

-- |
origin :: [Row] -> Position
origin rows = (1, length (head rows) `div` 2)

-- | Returns true if the game is over.
isGameOver :: [Row] -> Bool
isGameOver = any isJust . head