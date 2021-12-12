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
update (Action Swap) = step . swap
update _ = step

--------------------------------- Constructors ---------------------------------

tetris :: IO Tetris
tetris = spawn . Tetris newMatrix <$> randomPlayer