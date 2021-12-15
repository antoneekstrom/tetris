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
    SpecialKey (KeyDown, KeyEsc, KeyLeft, KeyRight, KeyShiftL, KeySpace, KeyUp),
  )
import Player (Player (queue, score, linesCleared), held, level)
import Rows (Row)
import System.Random (StdGen, newStdGen)
import Tetris (Tetris (time), newTetris, player, rows, step, stepR, stepTime, tetromino)
import Tetromino (Tetromino, color, landing, minos)
import Utilities (Color (..), Position, enumerate, enumerateMatrix)
import Prelude hiding (translate)

--------------------------------- Types ---------------------------------

type World = (Tetris, StdGen)

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
    w <- world
    play window black 60 w view input update

update :: Float -> World -> World
update t (tetris, g) = first (step . stepTime t) $ stepR g tetris

world :: IO World
world = (newTetris,) <$> newStdGen

view :: World -> Picture
view w =
  Pictures $
    map
      ($ fst w)
      ([ renderCells,
        renderLanding,
        renderCurrentTetromino,
        renderQueue,
        renderHold
      ] ++ renderText)

input :: Event -> World -> World
input e = first (Action.apply (actionFromInput e))

actionFromInput :: Event -> Action.Action
actionFromInput (EventKey key Down _ _) =
  case key of
    -- Move with arrow keys
    (SpecialKey KeyRight) -> Action.Input (Action.Move Action.Right)
    (SpecialKey KeyLeft) -> Action.Input (Action.Move Action.Left)
    (SpecialKey KeyDown) -> Action.Input (Action.Move Action.Down)
    -- Rotate
    (SpecialKey KeyUp) -> Action.Input (Action.Move Action.Rotate)
    (Char 'z') -> Action.Input (Action.Move Action.RotateCC)
    (Char 'x') -> Action.Input (Action.Move Action.Rotate)
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

renderText :: [Tetris -> Picture]
renderText = [renderScore, renderCleardLines, renderLevel]

renderScore :: Tetris -> Picture
renderScore = translate (-140) (-220) . Scale 0.1 0.1 . Text . ("Score: " ++) . show . score . player

renderCleardLines :: Tetris -> Picture
renderCleardLines = translate (-140) (-200) . Scale 0.1 0.1 . Text . ("Cleared lines: " ++) . show . linesCleared . player

renderLevel :: Tetris -> Picture
renderLevel = translate (-140) (-180) . Scale 0.1 0.1 . Text . ("Level: " ++) . show . level . player

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