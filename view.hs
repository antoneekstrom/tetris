{-# LANGUAGE TupleSections #-}

module View where

import Data.Maybe (fromMaybe)
import GHC.Float (int2Float)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (Event (EventKey), Key (Char, SpecialKey), KeyState (Down), SpecialKey (KeyDown, KeyLeft, KeyRight, KeyShiftL, KeySpace, KeyUp))
import Matrix
import Tetris
import Tetromino
import Utilities (enumerateMatrix)

cellSize = 30

windowSize = (10 * cellSize, 20 * cellSize)

window :: Display
window = InWindow "Tetris" windowSize (10, 10)

view :: Tetris -> Picture
view ts = Pictures [renderCells ts, maybe Blank renderTetromino (tetromino $ matrix ts)]

renderBlocks blocks = Pictures $ map (\((row, col), cell) -> renderBlock (col, row) (int2Float cellSize) (fromMaybe White cell)) blocks

renderCells ts = renderBlocks $ concat $ enumerateMatrix (rows $ matrix ts)

renderTetromino t = renderBlocks $ map (,Just $ Tetromino.color t) $ minos t

handleInput :: Event -> Tetris -> Tetris
handleInput (EventKey key Graphics.Gloss.Interface.IO.Game.Down modifiers _) ts =
  case key of
    (SpecialKey KeyRight) -> update (Action $ Move Tetris.Right) ts
    (SpecialKey KeyLeft) -> update (Action $ Move Tetris.Left) ts
    (SpecialKey KeyUp) -> update (Action $ Move Tetris.Rotate) ts
    (SpecialKey KeyDown) -> update (Control None) ts
    (SpecialKey KeySpace) -> update (Action Drop) ts
    (Char 'c') -> update (Action Swap) ts
    (SpecialKey KeyShiftL) -> update (Action Swap) ts
    _ -> ts
handleInput _ ts = ts

step :: Float -> Tetris -> Tetris
step i = update (Control None)

main :: IO ()
main =
  do
    ts <- tetris
    play window black 2 ts view handleInput View.step

renderBlock :: (Int, Int) -> Float -> Tetromino.Color -> Picture
renderBlock pos size color = Color (getColor color) $ View.translate pos size $ square size

translate :: (Int, Int) -> Float -> Picture -> Picture
translate (x, y) size =
  Translate
    (int2Float x * size - (int2Float (fst windowSize) / 2) + (size / 2))
    (int2Float (- y) * size + (int2Float (snd windowSize) / 2) - (size / 2))

square :: Float -> Picture
square size = Polygon $ rectanglePath size size

getColor :: Tetromino.Color -> Graphics.Gloss.Color
getColor Cyan = cyan
getColor Magenta = magenta
getColor Yellow = yellow
getColor Orange = orange
getColor Blue = blue
getColor Green = green
getColor Red = red
getColor White = white
getColor Black = black