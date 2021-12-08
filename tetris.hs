module Tetris where

import Data.Bifunctor (Bifunctor (bimap, first, second))
import Data.Maybe (isJust, isNothing, fromJust)

-- | A color.
data Color = Cyan | Magenta | Yellow | Orange | Blue | Green | Red | White
  deriving (Eq, Show)

-- | Value of a cell.
type Cell = Maybe Color

-- | Row of cells.
type Row = [Cell]

-- | A pair of ints.
type Vector a = (a, a)

-- | A position.
type Position = Vector Int

-- | A direction.
type Direction = Vector Int

-- | Piece of a tetromino.
type Mino = Position

type Rotate = Tetromino -> Tetromino

-- | Arrangement of squares which forms a shape.
data Tetromino = Tetromino {color :: Color, getRotate :: Rotate, getRotateCC :: Rotate, offsets :: [Position], center :: Position}

-- | Action which moves the current tetromino.
data Move = Left | Right | Down | Rotate | RotateCC

-- | Action by the player.
data Action = Move | Drop | Hold

-- | Input performed by player.
data Input = None | Action | Pause | Quit

-- | The board on which the game takes place.
data Matrix = Matrix {rows :: [Row], tetromino :: Maybe Tetromino}

-- | Queue of tetrominos.
type Queue = [Position -> Tetromino]

-- | The players score.
type Score = Int

-- | The state of the player.
data Player = Player {held :: Maybe Tetromino, canHold :: Bool, queue :: Queue, score :: Score}

-- | The state of the game.
data State = State Matrix Player

-- | A width and a height.
type Dimension = (Int, Int)

--------------------------------- Typeclasses ---------------------------------

instance Show Matrix where
  show (Matrix rows t) = concatMap ((++ "\n") . concatMap showCell) (mapWithPosition rows)
    where
      showCell :: (Position, Cell) -> String
      showCell (p, c) | maybe False (`contains` p) t = "o"
      showCell (_, Nothing) = "."
      showCell (_, Just _) = "*"

instance Show Tetromino where
  show t = show $ Matrix (matrix (5, 5)) (Just t)

--------------------------------- Utilities ---------------------------------

-- | Returns a new, empty matrix.
matrix :: Dimension -> [Row]
matrix (row, col) = replicate row (emptyRow col)

emptyRow :: Int -> Row
emptyRow n = replicate n Nothing

-- | Maps each element with its corresponding index.
enumerate :: (Num a, Enum a) => [b] -> [(a, b)]
enumerate = zip [0 ..]

-- | Maps each element with its corresponding position in the matrix.
mapWithPosition :: [[b]] -> [[(Position, b)]]
mapWithPosition rows = map (\(row, r) -> map (\(col, c) -> ((row, col), c)) r) $ enumerate (map enumerate rows)

-- | Returns true if the tetromino has a mino in the given position.
contains :: Tetromino -> Position -> Bool
contains t p = p `elem` minos t

(+.+) :: Num a => Vector a -> Vector a -> Vector a
(x1, y1) +.+ (x2, y2) = (x1 + x2, y1 + y2)

down :: Direction
down = (0, 1)

up :: Direction
up = (0, -1)

left :: Direction
left = (0, -1)

right :: Direction
right = (0, 1)

(!!=) :: [a] -> (a, Int) -> [a]
xs !!= (y, i) = uncurry (++) (second ((y :) . tail) $ splitAt i xs)

getCell :: [[a]] -> Position -> a
getCell rows (row, col) = rows !! row !! col

setCell :: [[a]] -> (a, Position) -> [[a]]
setCell rows (c, (row, col)) = rows !!= ((rows !! row) !!= (c, col), row)

isWithinBounds :: [[a]] -> Position -> Bool
isWithinBounds rows (row, col) = row >= 0 || row < length rows || col >= 0 || col < length (head rows)

isColliding :: [Row] -> Tetromino -> Bool
isColliding rows t = any (isJust . (rows `getCell`)) (minos t)

minos :: Tetromino -> [Mino]
minos t = map (center t +.+) (offsets t)

--------------------------------- Logic ---------------------------------

-- | Returns true if the tetromino can be moved.
isLegal :: (Tetromino -> Tetromino) -> [Row] -> Tetromino -> Bool
isLegal f rows t
  | not $ any (isWithinBounds rows) (minos t) = False
  | otherwise = not $ isColliding rows (f t)

canMove :: Direction -> [Row] -> Tetromino -> Bool
canMove d rows = isLegal (move d rows) rows

canRotate :: [Row] -> Tetromino -> Bool
canRotate = isLegal rotate

canRotateCC :: [Row] -> Tetromino -> Bool
canRotateCC = isLegal rotateCC

move :: Direction -> [Row] -> Tetromino -> Tetromino
move d rows t = t {center = center t +.+ d}

-- | Returns true if the tetromino can fall without colliding.
canFall :: [Row] -> Tetromino -> Bool
canFall = canMove down

-- | Turns a mino that has completed its falling into cells.
lock :: [Row] -> Tetromino -> [Row]
lock rows t = foldr (lockMino $ color t) rows (minos t)
  where
    lockMino :: Color -> Mino -> [Row] -> [Row]
    lockMino c m rows = setCell rows (Just c, m)

-- | Moves the tetromino down one step.
fall :: [Row] -> Tetromino -> ([Row], Maybe Tetromino)
fall rows t
  | canFall rows t = (rows, Just $ move down rows t)
  | otherwise = (lock rows t, Nothing)

-- | Returns the tetromino where it would be if it would fall.
dropLocation :: [Row] -> Tetromino -> Tetromino
dropLocation rows t
  | canFall rows t = uncurry dropLocation (second fromJust $ fall rows t)
  | otherwise = t

-- | Drops the tetromino until it lands and locks it instantly.
instantDrop :: [Row] -> Tetromino -> [Row]
instantDrop rows t = lock rows (dropLocation rows t)

initialPosition :: [Row] -> Position
initialPosition rows = (0, length (head rows) `div` 2)

resetPosition :: Matrix -> Matrix
resetPosition m@(Matrix rows Nothing) = error "No tetromino in matrix"
resetPosition m@(Matrix rows (Just t)) = m {tetromino = Just t {center = initialPosition rows}}

-- | Clears any full rows and returns the score gained.
clearRows :: [Row] -> ([Row], Score)
clearRows rows = (replicate cleared (emptyRow rowLen) ++ remaining, score $ length $ filter (not . all isJust) rows)
  where
    remaining = filter (not . all isJust) rows
    cleared = length rows - length remaining
    rowLen = length $ head rows
    score 1 = 40
    score 2 = 100
    score 3 = 300
    score 4 = 1200
    score _ = -1

isGameOver :: Matrix -> Bool
isGameOver = undefined

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

next :: Player -> (Position -> Tetromino)
next p = head $ queue p

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

allTetrominos :: [Position -> Tetromino]
allTetrominos = [i', t', o', j', l', s', z']

--------------------------------- State ---------------------------------

-- | Swaps the tetromino with the held one.
swap :: State -> State
swap s@(State m p)
  | canHold p = spawn $ State (resetPosition $ m {tetromino = held p}) p {canHold = False, held = tetromino m}
  | otherwise = s

-- | Spawns the next tetromino in the queue.
spawn :: State -> State
spawn s@(State m p)
  | isNothing (tetromino m) = State m {tetromino = Just $ next p (initialPosition $ rows m)} p {queue = tail $ queue p}
  | otherwise = s

-- | Applies the given move to the tetromino.
apply :: Matrix -> Input -> Matrix
apply m i = undefined

update :: Matrix -> Matrix
update m@(Matrix _ Nothing) = m
update (Matrix rows (Just t)) = uncurry Matrix (fall rows t)

newPlayer :: Player
newPlayer = Player {queue = allTetrominos, canHold = True, held = Nothing, score = 0}

newGame :: State
newGame = State (Matrix (matrix (20, 10)) $ Just $ t' (2, 4)) newPlayer