module Tetris where

import Data.Bifunctor (Bifunctor (second))
import Data.Maybe
import Matrix
import Player
import Tetromino
import Utilities
import System.Random (StdGen)

--------------------------------- Types ---------------------------------

-- | The state of the game.
data Tetris = Tetris
  { matrix :: Matrix,
    player :: Player
  }

-- | Action which moves the current tetromino.
data MoveAction = Left | Right | Down | Rotate | RotateCC

-- | Action by the player.
data ActionInput = Move MoveAction | Drop | Swap

-- | An action affecting the control flow of the game.
data ControlAction = None | Pause

-- | Input performed by player.
data Input = Control ControlAction | Action ActionInput

--------------------------------- Show ---------------------------------

instance Show Tetris where
  show (Tetris m p) = show m

--------------------------------- Functions ---------------------------------

spawn :: Tetris -> Tetris
spawn ts@(Tetris m p) =
  let (p', t) = fromQueue (rows m) p
   in case tetromino m of
        Nothing -> Tetris (current (Just t) m) p' {canHold = True}
        _ -> ts

swap :: Tetris -> Tetris
swap (Tetris m p) | not $ canHold p = Tetris m p
swap (Tetris m p) =
  let (pHolding, tHeld) = hold (rows m) (tetromino m) p
      (pSpawned, tSpawned) = fromQueue (rows m) pHolding
   in case tHeld of
        Nothing -> Tetris (current (Just tSpawned) m) pSpawned
        _ -> Tetris (current tHeld m) pHolding

rotate :: Tetris -> Tetris
rotate (Tetris m p) =
  case tetromino m of
    Just t -> Tetris m {tetromino = Just $ Tetromino.rotate t} p
    Nothing -> Tetris m p

rotateCC :: Tetris -> Tetris
rotateCC (Tetris m p) =
  case tetromino m of
    Just t -> Tetris m {tetromino = Just $ Tetromino.rotateCC t} p
    Nothing -> Tetris m p

move :: (Int, Int) -> Tetris -> Tetris
move dir (Tetris m p) =
  case tetromino m of
    Just t -> Tetris m {tetromino = Just $ Tetromino.move dir (rows m) t} p
    Nothing -> Tetris m p

drop :: Tetris -> Tetris
drop (Tetris m p) =
  case tetromino m of
    Just t -> spawn $ Tetris m {rows = Tetromino.drop (rows m) t, tetromino = Nothing} p
    Nothing -> Tetris m p

delete :: Tetris -> Tetris
delete (Tetris m p) = Tetris m {tetromino = Nothing} p

step :: Tetris -> Tetris
step ts@(Tetris m p) =
  case tetromino m of
    Nothing -> ts
    Just t -> spawn $ Tetris m {rows = rows''} {tetromino = t'} p {score = score p + clearedScore}
      where
        (rows'', clearedScore) = clearRows rows'
        (rows', t') = fall (rows m) t

refill :: StdGen -> Tetris -> Tetris
refill g (Tetris m p) = Tetris m p { queue = q }
  where
    (q, g') = refillQueue g (queue p)

update :: StdGen -> Input -> Tetris -> Tetris
update g (Action Swap) = refill g . swap
update g (Action (Move Rotate)) = refill g . Tetris.rotate
update g (Action (Move RotateCC)) = refill g . Tetris.rotateCC
update g (Action (Move Tetris.Left)) = refill g . Tetris.move left
update g (Action (Move Tetris.Right)) = refill g . Tetris.move right
update g (Action Drop) = refill g . Tetris.drop
update g _ = refill g . step

--------------------------------- Constructors ---------------------------------

tetris :: StdGen -> (Tetris, StdGen)
tetris g = (spawn $ Tetris newMatrix p, g')
  where
    (p, g') = randomPlayer g