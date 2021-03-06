module Tetris
  ( Tetris (rows, tetromino, player, isPaused),
    newTetris,
    step,
    stepR,
    Tetris.swap,
    move,
    Tetris.drop,
    rotate,
    rotateCC,
    fall,
    togglePause,
    stepTime,
    time,
  )
where

import Data.Bifunctor (Bifunctor (second))
import Data.Maybe (fromMaybe, isNothing)
import Player (Player (canHold), addLinesCleared, level, newPlayer, next, populateQueue, swap)
import Rows (Cell, Row, clear, emptyRows, isGameOver, origin)
import System.Random (StdGen)
import Tetromino (Tetromino, contains)
import qualified Tetromino (drop, fall, move, rotate, rotateCC)
import Utilities (Position, enumerateMatrix, matrix)

--------------------------------- Types ---------------------------------

-- | The state of the game.
data Tetris = Tetris
  { rows :: [Row],
    tetromino :: Maybe Tetromino,
    player :: Player,
    isPaused :: Bool,
    time :: Float
  }

--------------------------------- Show ---------------------------------

instance Show Tetris where
  show tetris = concatMap ((++ "\n") . concatMap showCell) (enumerateMatrix (rows tetris))
    where
      showCell :: (Position, Cell) -> String
      showCell (p, c) | maybe False (`contains` p) (tetromino tetris) = "o"
      showCell (_, Nothing) = "."
      showCell (_, Just _) = "*"

--------------------------------- Step ---------------------------------

-- | Steps the gameloop.
step :: Tetris -> Tetris
step tetris
  | isPaused tetris = tetris
  | otherwise = stepGameOver . stepSpawn . stepClear . stepT $ tetris

-- | Steps the random part of the gameloop.
stepR :: StdGen -> Tetris -> (Tetris, StdGen)
stepR = stepQueue

-- | Steps the time-dependent part of the gameloop.
stepT :: Tetris -> Tetris
stepT tetris
  | not $ isTime tetris = tetris
  | otherwise = consumeTime . stepFall $ tetris

-- |
stepFall :: Tetris -> Tetris
stepFall tetris = fromMaybe tetris tetris'
  where
    r = Tetromino.fall (rows tetris) <$> tetromino tetris
    tetris' = (\(rows, t) -> tetris {rows = rows, tetromino = t}) <$> r

-- |
stepGameOver :: Tetris -> Tetris
stepGameOver tetris
  | isGameOver (rows tetris) = newTetris
  | otherwise = tetris

-- |
stepQueue :: StdGen -> Tetris -> (Tetris, StdGen)
stepQueue g tetris = (tetris {player = p}, g')
  where
    (p, g') = populateQueue g (player tetris)

-- |
stepClear :: Tetris -> Tetris
stepClear tetris = tetris {rows = rows', player = addLinesCleared nCleared (player tetris)}
  where
    (rows', nCleared) = clear (rows tetris)

-- |
stepSpawn :: Tetris -> Tetris
stepSpawn = stepP (isNothing . tetromino) f
  where
    f tetris = tetris'
      where
        (p', tf) = next (player tetris)
        t = tf $ origin (rows tetris)
        tetris' = tetris {player = p', tetromino = Just t}

-- | Increases the time by the given amount.
stepTime :: Float -> Tetris -> Tetris
stepTime t tetris
  | isPaused tetris = tetris
  | otherwise = tetris {time = time tetris + t}

-- |
stepP :: (p -> Bool) -> (p -> p) -> p -> p
stepP p f tetris
  | p tetris = f tetris
  | otherwise = tetris

--------------------------------- Actions ---------------------------------

-- |
swap :: Tetris -> Tetris
swap tetris | not (canHold $ player tetris) = tetris
swap tetris = tetris'
  where
    (p, tf) = Player.swap (tetromino tetris) (player tetris)
    t = tf (origin $ rows tetris)
    tetris' = tetris {player = p, tetromino = Just t}

-- |
move :: (Int, Int) -> Tetris -> Tetris
move direction tetris = tetris'
  where
    t = Tetromino.move direction (rows tetris) <$> tetromino tetris
    tetris' = tetris {tetromino = t}

-- |
drop :: Tetris -> Tetris
drop tetris = fromMaybe tetris tetris'
  where
    r = Tetromino.drop (rows tetris) <$> tetromino tetris
    tetris' = (\rows' -> tetris {rows = rows', tetromino = Nothing}) <$> r

-- |
rotate :: Tetris -> Tetris
rotate tetris = tetris'
  where
    t = Tetromino.rotate (rows tetris) <$> tetromino tetris
    tetris' = tetris {tetromino = t}

-- |
rotateCC :: Tetris -> Tetris
rotateCC tetris = tetris'
  where
    t = Tetromino.rotateCC (rows tetris) <$> tetromino tetris
    tetris' = tetris {tetromino = t}

-- |
togglePause :: Tetris -> Tetris
togglePause tetris = tetris {isPaused = not (isPaused tetris)}

-- |
fall :: Tetris -> Tetris
fall = stepFall

-- |
isTime :: Tetris -> Bool
isTime tetris = time tetris >= timeStep tetris

-- |
timeStep :: Tetris -> Float
timeStep tetris = max (2 / frameRate) $ (frameRate - (l * 2)) / frameRate
  where
    frameRate = 60
    l = fromIntegral $ level $ player tetris

-- |
consumeTime :: Tetris -> Tetris
consumeTime tetris = tetris {time = time tetris - timeStep tetris}

--------------------------------- Constructors ---------------------------------

-- |
newTetris :: Tetris
newTetris = Tetris emptyRows Nothing newPlayer False 0

--------------------------------- QuickCheck ---------------------------------