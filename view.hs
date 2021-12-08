{-# LANGUAGE TupleSections #-}
module View where

import Data.Maybe (maybe, isJust, fromJust, fromMaybe)
import Graphics.Gloss
import Tetris
import Data.Bifunctor (Bifunctor(second))

getColor :: Tetris.Color -> Graphics.Gloss.Color
getColor Cyan = cyan
getColor Magenta = magenta
getColor Yellow = yellow
getColor Orange = orange
getColor Blue = blue
getColor Green = green
getColor Red = red
getColor White = white

renderBlock :: Position -> Graphics.Gloss.Color -> Picture
renderBlock pos c = colorCell c $ translateCell pos rect
    where
        colorCell c = Color c
        translateCell (row, col) = Translate (fromIntegral row * 10) (fromIntegral col * 10)
        rect = Polygon $ rectanglePath 10 10

renderBlocks :: [(Position, Tetris.Color)] -> [Picture]
renderBlocks = map $ uncurry renderBlock . second getColor

renderMatrix :: [Row] -> [Picture]
renderMatrix rows = renderBlocks $ map (second $ fromMaybe White) $ concat $ mapWithPosition rows

renderTetromino :: Tetromino -> [Picture]
renderTetromino t = renderBlocks $ map (,Tetris.color t) (minos t)

view :: State -> Picture
view (State (Matrix rows Nothing) _) = Pictures $ renderMatrix rows
view (State (Matrix rows (Just t)) _) = Pictures $ renderMatrix rows ++ renderTetromino t

handleInput e s = s

step _ (State m t) = State (update m) t

main = play window black 2 newGame view handleInput step
  where
    window = InWindow "Tetris" (600, 600) (10, 10)

test = uncurry Matrix $ fall (matrix (20, 10)) (t' (2, 4))