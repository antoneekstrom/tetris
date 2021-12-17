module Show where

import Data.Maybe (fromJust, fromMaybe, isJust)
import Rows (Row, emptyRows)
import Tetromino (Tetromino, contains)
import Utilities (Dimension, Position, enumerateMatrix, matrix)

--------------------------------- Types ---------------------------------

-- |
newtype TetrominoFactory = TetrominoFactory (Position -> Tetromino)

--------------------------------- Helpers ---------------------------------

showTetris :: [Row] -> Tetromino -> [Char]
showTetris rows t = concatMap ((++ "\n") . map show') $ enumerateMatrix rows
  where
    show' (p, c)
      | isJust $ showMino p t = fromJust $ showMino p t
      | otherwise = showCell (p, c)

showMino :: Position -> Tetromino -> Maybe Char
showMino p t
  | t `contains` p = Just 'o'
  | otherwise = Nothing

showCell :: (a1, Maybe a2) -> Char
showCell (_, Just _) = '*'
showCell (_, Nothing) = '.'

--------------------------------- Classes ---------------------------------

instance Show TetrominoFactory where
  show (TetrominoFactory f) = showTetris (matrix (5, 5) Nothing) $ f (2, 2)

instance Show Tetromino where
  show = showTetris emptyRows