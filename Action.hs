module Action
  ( apply,
    Action (..),
    ControlAction (..),
    InputAction (..),
    MoveAction (..),
  )
where

import Tetris (Tetris (isPaused), drop, fall, move, rotate, rotateCC, step, swap, togglePause)
import Utilities (left, right)

--------------------------------- Types ---------------------------------

-- | Action which moves the current tetromino.
data MoveAction = Left | Right | Down | Rotate | RotateCC

-- | Action by the player.
data InputAction = Move MoveAction | Drop | Swap

-- | An action affecting the control flow of the game.
data ControlAction = None | Pause

-- | Input performed by player.
data Action = Control ControlAction | Input InputAction

--------------------------------- Functions ---------------------------------

apply :: Action -> Tetris -> Tetris
apply (Control Pause) tetris = Tetris.togglePause tetris
apply a tetris
  | isPaused tetris = tetris
  | otherwise = apply' a tetris

apply' :: Action -> Tetris -> Tetris
apply' (Input Swap) = swap
apply' (Input (Move Action.Left)) = Tetris.move left
apply' (Input (Move Action.Right)) = Tetris.move right
apply' (Input (Move Action.Down)) = Tetris.fall
apply' (Input (Move Rotate)) = Tetris.rotate
apply' (Input (Move RotateCC)) = Tetris.rotateCC
apply' (Input Drop) = Tetris.drop
apply' _ = id