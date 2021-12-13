{-# LANGUAGE TupleSections #-}

module Main where

import qualified Action
  ( Action (Control, Input),
    ControlAction (..),
    InputAction (..),
    MoveAction (..),
    apply,
  )
import Data.Bifunctor (Bifunctor (bimap, first, second))
import Data.Maybe (fromMaybe)
import Graphics.Gloss hiding (translate)
import Graphics.Gloss.Interface.IO.Game
  ( Event (EventKey),
    Key (Char, SpecialKey),
    KeyState (Down),
    SpecialKey (KeyDown, KeyLeft, KeyRight, KeyShiftL, KeySpace, KeyUp, KeyEsc),
  )
import Player (Player (queue), held)
import Rows (Row)
import System.Random (StdGen, newStdGen)
import Tetris (Tetris, newTetris, player, rows, step, stepR, tetromino)
import Tetromino (Tetromino, color, landing, minos)
import Utilities (Color (..), Position, enumerate, enumerateMatrix)
import Prelude hiding (translate)

-- handleInput :: StdGen -> Event -> Tetris -> Tetris
-- handleInput g (EventKey key Graphics.Gloss.Interface.IO.Game.Down modifiers _) ts =
--   case key of
--     (SpecialKey KeyRight) -> update g (Action $ Move Right) ts
--     (SpecialKey KeyLeft) -> update g (Action $ Move Left) ts
--     (SpecialKey KeyUp) -> update g (Action $ Move Rotate) ts
--     (SpecialKey KeyDown) -> update g (Control None) ts
--     (SpecialKey KeySpace) -> update g (Action Drop) ts
--     (Char 'c') -> update g (Action Swap) ts
--     (SpecialKey KeyShiftL) -> update g (Action Swap) ts
--     _ -> ts
-- handleInput _ _ ts = ts

--------------------------------- Constants ---------------------------------

cellSize :: Int
cellSize = 30

previewSize :: Int
previewSize = 8

windowSize :: (Int, Int)
windowSize = (10 * cellSize, 20 * cellSize)

window :: Display
window = InWindow "Tetris" windowSize (10, 10)

--------------------------------- Main ---------------------------------

main :: IO ()
main =
  do
    g <- newStdGen
    play window black 2 world view input (update' g)
  where
    update' g = \f tetris -> fst $ update g f tetris

update :: StdGen -> Float -> Tetris -> (Tetris, StdGen)
update g _ = first Tetris.step . Tetris.stepR g

world :: Tetris
world = newTetris

view :: Tetris -> Picture
view tetris =
  Pictures $
    map
      ($ tetris)
      [ renderCells,
        renderLanding,
        renderCurrentTetromino,
        renderQueue,
        renderHold
      ]

input :: Event -> Tetris -> Tetris
input e = Action.apply (actionFromInput e)

actionFromInput :: Event -> Action.Action
actionFromInput (EventKey key Down _ _) =
  case key of
    -- Move with arrow keys
    (SpecialKey KeyRight) -> Action.Input (Action.Move Action.Right)
    (SpecialKey KeyLeft) -> Action.Input (Action.Move Action.Left)
    (SpecialKey KeyUp) -> Action.Input (Action.Move Action.Rotate)
    (SpecialKey KeyDown) -> Action.Input (Action.Move Action.Down)
    -- Hold / Swap
    (SpecialKey KeyShiftL) -> Action.Input Action.Swap
    (Char 'c') -> Action.Input Action.Swap
    -- Drop
    (SpecialKey KeySpace) -> Action.Input Action.Drop
    -- Pause
    (Char 'p') -> Action.Control Action.Pause
    (Char 'q') -> Action.Control Action.Pause
    _ -> Action.Control Action.None
actionFromInput _ = Action.Control Action.None

--------------------------------- View ---------------------------------

renderCells :: Tetris -> Picture
renderCells tetris = renderBlocks cellSize $ map (second (maybe white getColor)) $ concat $ enumerateMatrix (rows tetris)

renderCurrentTetromino :: Tetris -> Picture
renderCurrentTetromino tetris = maybe Blank (renderTetromino cellSize) (tetromino tetris)

renderQueue :: Tetris -> Picture
renderQueue tetris =
  translate (fromIntegral cellSize) (fromIntegral cellSize) $
    Pictures $
      map (\(i, tf) -> renderTetromino previewSize (tf (0, i * 4))) $
        enumerate (take 5 $ queue $ player tetris)

renderHold :: Tetris -> Picture
renderHold tetris =
  translate
    (fromIntegral (fst windowSize) - 1.2 * fromIntegral cellSize)
    (fromIntegral cellSize)
    $ maybe Blank (\tf -> renderTetromino previewSize (tf (0, 0))) (held $ player tetris)

renderLanding :: Tetris -> Picture
renderLanding tetris = maybe Blank (renderTetromino' (greyN 0.8) cellSize) landed
  where
    landed = landing (rows tetris) <$> tetromino tetris

--------------------------------- Helpers ---------------------------------

renderTetromino :: Int -> Tetromino -> Picture
renderTetromino size t = renderTetromino' (getColor $ Tetromino.color t) size t

renderTetromino' :: Graphics.Gloss.Color -> Int -> Tetromino -> Picture
renderTetromino' color size t = renderBlocks size $ map (,color) $ minos t

renderBlocks :: Int -> [(Position, Graphics.Gloss.Color)] -> Picture
renderBlocks size blocks = Pictures $ map (\((row, col), cell) -> renderBlock (col, row) size cell) blocks

renderBlock :: (Int, Int) -> Int -> Graphics.Gloss.Color -> Picture
renderBlock pos size color = Color color $ uncenter size' $ translate' pos' $ square size'
  where
    f = fromIntegral . (* size)
    pos' = bimap f f pos
    size' = fromIntegral size

uncenter :: Float -> Picture -> Picture
uncenter size =
  translate' (bimap f f windowSize)
  where
    f = (+ (size / 2)) . (/ 2) . fromIntegral . negate

translate :: Float -> Float -> Picture -> Picture
translate x y = Translate x (- y)

translate' :: (Float, Float) -> Picture -> Picture
translate' = uncurry translate

square :: Float -> Picture
square size = Polygon $ rectanglePath size size

getColor :: Utilities.Color -> Graphics.Gloss.Color
getColor Cyan = cyan
getColor Magenta = magenta
getColor Yellow = yellow
getColor Orange = orange
getColor Blue = blue
getColor Green = green
getColor Red = red