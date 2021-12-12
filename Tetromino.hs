module Tetromino where

import Data.Bifunctor (Bifunctor (second))
import Data.Maybe (fromJust)
import Maybes (isJust)
import Utilities (Position, addp, getm, setm, withinBounds, At, at)

--------------------------------- Types ---------------------------------

-- | Arrangement of squares which forms a shape.
data Tetromino = Tetromino
  { color :: Color,
    getRotate :: Rotate,
    getRotateCC :: Rotate,
    offsets :: [Position],
    center :: Position
  }

-- | A color.
data Color = Cyan | Magenta | Yellow | Orange | Blue | Green | Red | White | Black
  deriving (Eq, Show)

-- | Piece of a tetromino.
type Mino = (Int, Int)

-- | Rotates a tetromino.
type Rotate = Tetromino -> Tetromino

-- | Value of a cell.
type Cell = Maybe Color

-- | Row of cells.
type Row = [Cell]

--------------------------------- At ---------------------------------

instance At Tetromino where
  t `at` c = t { center = c }

--------------------------------- Functions ---------------------------------

-- | Returns the absolute positions of the squares in the tetromino.
minos :: Tetromino -> [Mino]
minos t = map (center t `addp`) (offsets t)

-- | Returns true if the tetromino has a mino in the given position.
contains :: Tetromino -> Position -> Bool
contains t p = p `elem` minos t

isColliding :: [Row] -> Tetromino -> Bool
isColliding rows t = any (isJust . (rows `getm`)) (minos t)

-- | Returns true if the tetromino can be moved.
isLegal :: (Tetromino -> Tetromino) -> [Row] -> Tetromino -> Bool
isLegal f rows t = all (withinBounds rows) (minos t') && not (isColliding rows t')
  where
    t' = f t

-- |
translate :: (Int, Int) -> [Row] -> Tetromino -> Tetromino
translate d rows t = t {center = center t `addp` d}

-- |
canMove :: (Int, Int) -> [Row] -> Tetromino -> Bool
canMove d rows = isLegal (translate d rows) rows

move :: (Int, Int) -> [Row] -> Tetromino -> Tetromino
move dir rows t
  | dir == down || dir == up = error "move: Can only move left and right."
  | canMove dir rows t = translate dir rows t
  | otherwise = t

--------------------------------- Matrix ---------------------------------

-- | Moves the tetromino down.
fall :: [Row] -> Tetromino -> ([Row], Maybe Tetromino)
fall rows t
  | canMove down rows t = (rows, Just $ translate down rows t)
  | otherwise = (lock rows t, Nothing)

-- | Returns the tetromino where it would be if it would fall all the way down.
landing :: [Row] -> Tetromino -> Tetromino
landing rows t =
  let (rows', t') = fall rows t
  in case t' of
    Nothing -> t
    Just t'' -> landing rows' t''

-- | Fills the matrix with the tetromino.
lock :: [Row] -> Tetromino -> [Row]
lock rows t = foldr (lockMino $ color t) rows (minos t)
  where
    lockMino :: Color -> Mino -> [Row] -> [Row]
    lockMino c m rows = setm rows (Just c, m)

-- | Drops a tetromino.
drop :: [Row] -> Tetromino -> [Row]
drop rows t = lock rows (landing rows t)

--------------------------------- Rotation ---------------------------------

rotatePivot' :: Bool -> Tetromino -> Tetromino
rotatePivot' cc t = t {offsets = map rotate (offsets t)}
  where
    rotate :: Position -> Position
    rotate (row, col) = (col, - row)

rotatePivot :: Tetromino -> Tetromino
rotatePivot = rotatePivot' False

rotatePivotCC :: Tetromino -> Tetromino
rotatePivotCC = rotatePivot' True

rotate :: Tetromino -> Tetromino
rotate t = getRotate t t

rotateCC :: Tetromino -> Tetromino
rotateCC t = getRotateCC t t

--------------------------------- Tetrominos ---------------------------------

-- | The I shaped tetromino.
i' :: Position -> Tetromino
i' = Tetromino Cyan rotatePivot rotatePivotCC [(-1, 0), (0, 0), (1, 0), (2, 0)]

t' :: Position -> Tetromino
t' = Tetromino Magenta rotatePivot rotatePivotCC [(0, 0), (-1, 0), (0, 1), (0, -1)]

o' :: Position -> Tetromino
o' = Tetromino Yellow id id [(0, 0), (1, 0), (0, 1), (1, 1)]

j' :: Position -> Tetromino
j' = Tetromino Orange rotatePivot rotatePivotCC [(1, 0), (0, 0), (1, 1), (2, 1)]

l' :: Position -> Tetromino
l' = Tetromino Blue rotatePivot rotatePivotCC [(0, 0), (1, 0), (2, 0), (1, 1)]

s' :: Position -> Tetromino
s' = Tetromino Green rotatePivot rotatePivotCC [(1, 0), (0, 0), (1, -1), (0, 1)]

z' :: Position -> Tetromino
z' = Tetromino Red rotatePivot rotatePivotCC [(1, 0), (0, 0), (1, 1), (0, -1)]

tetrominos :: [Position -> Tetromino]
tetrominos = [i', t', o', j', l', s', z']

--------------------------------- Directions ---------------------------------

down :: Num a => (a, a)
down = (1, 0)

up :: Num a => (a, a)
up = (-1, 0)

left :: Num a => (a, a)
left = (0, -1)

right :: Num a => (a, a)
right = (0, 1)