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
   in case held p of
        Nothing -> Tetris (current (Just t) m) p'
        _ -> ts

swap :: Tetris -> Tetris
swap ts@(Tetris m p) =
  let (p', held) = unhold p
      holdCurrent = hold (rows m) (tetromino m) p'
      (spawned, t) = fromQueue (rows m) holdCurrent
   in case held of
        Nothing -> Tetris (current (Just t) m) spawned
        Just _ -> Tetris (current held m) holdCurrent

delete :: Tetris -> Tetris
delete (Tetris m p) = Tetris m { tetromino = Nothing } p

step :: Tetris -> Tetris
step ts@(Tetris m p) =
  case tetromino m of
    Just t -> Tetris m { tetromino = Just $ fall (rows m) t } p
    Nothing -> ts

update :: Input -> Tetris -> Tetris
update (Action Swap) = step . swap
update _ = step

--------------------------------- Constructors ---------------------------------

tetris :: IO Tetris
tetris = spawn . Tetris newMatrix <$> randomPlayer