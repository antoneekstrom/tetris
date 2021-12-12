module Tetris where

import Data.Bifunctor (Bifunctor (second))
import Data.Maybe
import Matrix
import Player
import Tetromino
import Utilities

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
        Nothing -> Tetris (current (Just t) m) p'
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
    Just t -> spawn $ Tetris m {rows = rows'} {tetromino = t'} p
      where
        (rows', t') = fall (rows m) t

update :: Input -> Tetris -> Tetris
update (Action Swap) = swap
update (Action (Move Rotate)) = Tetris.rotate
update (Action (Move RotateCC)) = Tetris.rotateCC
update (Action (Move Tetris.Left)) = Tetris.move left
update (Action (Move Tetris.Right)) = Tetris.move right
update (Action Drop) = Tetris.drop
update _ = step

--------------------------------- Constructors ---------------------------------

tetris :: IO Tetris
tetris = spawn . Tetris newMatrix <$> randomPlayer