module Matrix where

import Data.Maybe (isJust)
import Tetromino
import Utilities

--------------------------------- Types ---------------------------------

-- | The matrix on which the game takes place.
data Matrix = Matrix
  { rows :: [Row],
    tetromino :: Maybe Tetromino
  }

-- | The players score.
type Score = Int

--------------------------------- Show ---------------------------------

instance Show Matrix where
  show (Matrix rows t) = concatMap ((++ "\n") . concatMap showCell) (enumerateMatrix rows)
    where
      showCell :: (Position, Cell) -> String
      showCell (p, c) | maybe False (`contains` p) t = "o"
      showCell (_, Nothing) = "."
      showCell (_, Just _) = "*"

--------------------------------- Functions ---------------------------------

-- | Clears any full rows and returns the score gained.
clearRows :: [Row] -> ([Row], Score)
clearRows rows = (replicate cleared (replicate rowLen Nothing) ++ remaining, score $ length $ filter (not . all isJust) rows)
  where
    remaining = filter (not . all isJust) rows
    cleared = length rows - length remaining
    rowLen = length $ head rows
    score 1 = 40
    score 2 = 100
    score 3 = 300
    score 4 = 1200
    score _ = 0

-- |
origin :: [Row] -> Position
origin rows = (3, length (head rows) `div` 2)

isGameOver :: Matrix -> Bool
isGameOver = undefined

current :: Maybe Tetromino -> Matrix -> Matrix
current t m = m {tetromino = t}

--------------------------------- Constructors ---------------------------------

newMatrix :: Matrix
newMatrix = Matrix (Utilities.matrix (20, 10) Nothing) Nothing
