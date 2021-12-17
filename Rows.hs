module Rows where

import Data.Maybe (isJust)
import Utilities (Color (..), Position)

--------------------------------- Types ---------------------------------

-- | Value of a cell.
type Cell = Maybe Color

-- | Row of cells.
type Row = [Cell]

--------------------------------- Functions ---------------------------------

-- | Clears any full rows and returns the score gained.
clear :: [Row] -> ([Row], Int)
clear rows = (replicate cleared (replicate rowLen Nothing) ++ remaining, cleared)
  where
    remaining = filter (not . all isJust) rows
    cleared = length rows - length remaining
    rowLen = length $ head rows

-- |
origin :: [Row] -> Position
origin rows = (3, length (head rows) `div` 2)

-- | Returns true if the game is over.
isGameOver :: [Row] -> Bool
isGameOver = any isJust . head

-- | Returns true if the given position is a cell.
rows `isCell` (r, c) = isJust col
  where
    row = rows !! r
    col = row !! c

--------------------------------- Constructors ---------------------------------

emptyRows' :: Int -> Int -> [Row]
emptyRows' rows cols = replicate rows (replicate cols Nothing)

emptyRows :: [Row]
emptyRows = emptyRows' 20 10

--------------------------------- Properties ---------------------------------

prop_clear_size :: [Row] -> Bool
prop_clear_size rows = length rows == length (fst $ clear rows)