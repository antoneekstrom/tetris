module Tetromino
  ( Tetromino,
    tetrominos,
    fall,
    color,
    minos,
    contains,
    move,
    Tetromino.drop,
    rotate,
    rotateCC,
    landing,
    tetrominoProps,
  )
where

import Data.Bifunctor (Bifunctor (second))
import Maybes (isJust)
import Rows (Row, emptyRows, isCell, origin)
import Utilities (At, Color (..), Position, addp, at, down, enumerateMatrix, getm, matrix, setm, up, withinBounds)

--------------------------------- Types ---------------------------------

-- | Arrangement of squares which forms a shape.
data Tetromino = Tetromino
  { color :: Color,
    getRotate :: Rotate,
    getRotateCC :: Rotate,
    offsets :: [Position],
    center :: Position
  }

-- | Piece of a tetromino.
type Mino = (Int, Int)

-- | Rotates a tetromino.
type Rotate = Tetromino -> Tetromino

--------------------------------- Classes ---------------------------------

instance At Tetromino where
  t `at` c = t {center = c}

instance Eq Tetromino where
  t1 == t2 =
    color t1 == color t2
      && offsets t1 == offsets t2
      && center t1 == center t2

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

ifLegal :: (Tetromino -> Tetromino) -> [Row] -> Tetromino -> Tetromino
ifLegal f rows t = if isLegal f rows t then t' else t
  where
    t' = f t

-- | Translates the center of a tetromino.
translate :: (Int, Int) -> Tetromino -> Tetromino
translate d t = t {center = center t `addp` d}

-- | Returns true if the tetromino can be moved.
canMove :: (Int, Int) -> [Row] -> Tetromino -> Bool
canMove d = isLegal (translate d)

-- | Moves a tetromino.
move :: (Int, Int) -> [Row] -> Tetromino -> Tetromino
move dir rows t
  | dir == down || dir == up = error "move: Can only move left and right."
  | otherwise = ifLegal (translate dir) rows t

-- | Moves the tetromino down.
fall :: [Row] -> Tetromino -> ([Row], Maybe Tetromino)
fall rows t
  | canMove down rows t = (rows, Just $ translate down t)
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
    rotate (row, col) = if cc then (- col, row) else (col, - row)

rotatePivot :: Tetromino -> Tetromino
rotatePivot = rotatePivot' False

rotatePivotCC :: Tetromino -> Tetromino
rotatePivotCC = rotatePivot' True

rotate :: [Row] -> Tetromino -> Tetromino
rotate rows t = ifLegal (getRotate t) rows t

rotateCC :: [Row] -> Tetromino -> Tetromino
rotateCC rows t = ifLegal (getRotateCC t) rows t

--------------------------------- Tetrominos ---------------------------------

i' :: Position -> Tetromino
i' = Tetromino Cyan rotatePivot rotatePivotCC [(-1, 0), (0, 0), (1, 0), (2, 0)]

t' :: Position -> Tetromino
t' = Tetromino Magenta rotatePivot rotatePivotCC [(0, 0), (-1, 0), (0, 1), (0, -1)]

o' :: Position -> Tetromino
o' = Tetromino Yellow id id [(0, 0), (1, 0), (0, 1), (1, 1)]

j' :: Position -> Tetromino
j' = Tetromino Orange rotatePivot rotatePivotCC [(0, -1), (0, 0), (0, 1), (-1, 1)]

l' :: Position -> Tetromino
l' = Tetromino Blue rotatePivot rotatePivotCC [(0, -1), (0, 0), (0, 1), (1, 1)]

s' :: Position -> Tetromino
s' = Tetromino Green rotatePivot rotatePivotCC [(1, 0), (0, 0), (1, -1), (0, 1)]

z' :: Position -> Tetromino
z' = Tetromino Red rotatePivot rotatePivotCC [(1, 0), (0, 0), (1, 1), (0, -1)]

tetrominos :: [Position -> Tetromino]
tetrominos = [i', t', o', j', l', s', z']

--------------------------------- Properties ---------------------------------

-- | Returns true if the tetromino is in the same position after being rotated four times.
prop_rotate_id :: Tetromino -> Bool
prop_rotate_id t = t == rcc (rcc (rcc (rcc t)))
  where
    rows = emptyRows
    rcc = rotateCC rows

-- | Returns true if the tetromino is in the same position after being rotated clockwise and then counter-clockwise.
prop_rotate_flip :: Tetromino -> Bool
prop_rotate_flip t = t == rcc (r t)
  where
    rows = emptyRows
    r = rotate rows
    rcc = rotateCC rows

prop_lock :: [Row] -> Tetromino -> Bool
prop_lock rows t = all (isCell rows') (minos t)
  where
    rows' = lock rows t

tetrominoProps :: [Tetromino -> Bool]
tetrominoProps = [prop_rotate_id, prop_rotate_flip, prop_lock emptyRows]