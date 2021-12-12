module Main where

import Matrix
import Player (Player (held, canHold))
import Tetris
import Tetromino
import Util
import Utilities

instance Show Tetromino where
  show t = show $ Matrix (Utilities.matrix (5, 5) Nothing) (Just t)

main = loop =<< tetris
  where
    loop ts =
      do
        print ts
        input <- getLine

        log input ts

        let update = handleInput input ts
        loop $ update ts

    handleInput input ts = case input of
      "swap" -> update (Action Swap)
      "left" -> update (Action (Move Tetris.Left))
      "right" -> update (Action (Move Tetris.Right))
      "rotate" -> update (Action (Move Rotate))
      "rotateCC" -> update (Action (Move RotateCC))
      "drop" -> update (Action Drop)
      _ -> update (Control None)

    log input ts =
      case input of
        "held" -> print $ held $ player ts
        "canHold" -> print $ canHold $ player ts
        _ -> return ()