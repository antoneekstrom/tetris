module Utilities where

import Data.Array (Ix (inRange))
import Data.Bifunctor (Bifunctor (second))
import System.Random (Random (randomR), StdGen)

--------------------------------- Types ---------------------------------

-- | A x and y position.
type Position = (Int, Int)

-- | A width and a height.
type Dimension = (Int, Int)

-- | A type which can be placed at a given position.
class At a where
  at :: a -> Position -> a

--------------------------------- Lists ---------------------------------

-- | Maps each element with its corresponding index.
enumerate :: (Num a, Enum a) => [b] -> [(a, b)]
enumerate = zip [0 ..]

-- | Removes the element at the given position in the list.
removeAt :: Int -> [a] -> ([a], a)
removeAt i xs = (uncurry (++) (second tail $ splitAt i xs), xs !! i)

-- | Shuffles a list using the fisher-yates shuffle.
shuffle :: StdGen -> [a] -> ([a], StdGen)
shuffle g [] = ([], g)
shuffle g xs = (aux g xs [], g)
  where
    aux g [] result = result
    aux g xs result = aux g' next (removed : result)
      where
        (next, removed) = removeAt i xs
        (i, g') = randomR (0, length xs - 1) g

--------------------------------- Matrices ---------------------------------

-- | Returns a new matrix.
matrix :: Dimension -> a -> [[a]]
matrix (row, col) a = replicate row (replicate col a)

-- | Maps each element with its corresponding position in the matrix.
enumerateMatrix :: [[b]] -> [[(Position, b)]]
enumerateMatrix rows = map (\(row, r) -> map (\(col, c) -> ((row, col), c)) r) $ enumerate (map enumerate rows)

-- | Sets the value at the given index in the list.
(!!=) :: [a] -> (a, Int) -> [a]
xs !!= (a, i) = uncurry (++) (second ((a :) . tail) $ splitAt i xs)

-- | Returns the element at the given position in the matrix.
getm :: [[a]] -> (Int, Int) -> a
getm rows (row, col) = rows !! row !! col

-- | Sets the element at the given position in the matrix.
setm :: [[a]] -> (a, (Int, Int)) -> [[a]]
setm rows (c, (row, col)) = rows !!= ((rows !! row) !!= (c, col), row)

-- | Adds two positions.
addp :: Num a => (a, a) -> (a, a) -> (a, a)
(x1, y1) `addp` (x2, y2) = (x1 + x2, y1 + y2)

-- | Returns true if the given position is within the range of the matrix.
withinBounds :: [[a]] -> Position -> Bool
withinBounds rows (row, col) =
  inRange (0, length rows - 1) row
    && inRange (0, length (rows !! row) - 1) col
