module Player
  ( Player (queue),
    newPlayer,
    swap,
    next,
    populateQueue,
    canHold,
    held,
    addLinesCleared,
    level,
    score,
    linesCleared,
  )
where

import Data.Bifunctor (Bifunctor (first, second))
import Rows (Row, origin)
import System.Random (StdGen)
import Tetromino (Tetromino, tetrominos)
import Utilities (At (at), Position, shuffle)

--------------------------------- Types ---------------------------------

-- | The state of the player.
data Player = Player
  { held :: Maybe TFactory,
    canHold :: Bool,
    queue :: Queue,
    linesCleared :: Int,
    score :: Score
  }

-- |
type Queue = [TFactory]

-- |
type TFactory = Position -> Tetromino

-- |
type Score = Int

-- |
type Level = Int

--------------------------------- Queue ---------------------------------

-- | Refills the queue with new tetrominos if neccessary.
refill :: StdGen -> Queue -> (Queue, StdGen)
refill g q
  | length q < length tetrominos = first (q ++) $ random g
  | otherwise = (q, g)

-- | Returns the next tetromino in the queue.
pop :: Queue -> (Queue, Position -> Tetromino)
pop q = (tail q, head q)

-- | Returns a randomly shuffled queue with one of each tetromino.
random :: StdGen -> (Queue, StdGen)
random g = shuffle g tetrominos

--------------------------------- Player ---------------------------------

-- |
swap :: Maybe Tetromino -> Player -> (Player, TFactory)
swap tCurrent p =
  let held = hold tCurrent p
   in case snd held of
        Nothing -> next (fst held)
        Just t -> (fst held, t)

-- |
hold :: Maybe Tetromino -> Player -> (Player, Maybe TFactory)
hold _ p | not $ canHold p = error "hold: can't hold"
hold t p = case t of
  Just t' -> (p {held = Just $ factory t', canHold = False}, held p)
  Nothing -> (p {canHold = False}, held p)

-- |
next :: Player -> (Player, TFactory)
next p = (p {queue = q, canHold = True}, t)
  where
    (q, t) = pop (queue p)

-- |
populateQueue :: StdGen -> Player -> (Player, StdGen)
populateQueue g p = (p {queue = q}, g')
  where
    (q, g') = refill g (queue p)

-- |
scoreFromLines :: Level -> Int -> Score
scoreFromLines l 1 = 40 * (l + 1)
scoreFromLines l 2 = 120 * (l + 1)
scoreFromLines l 3 = 300 * (l + 1)
scoreFromLines l 4 = 1200 * (l + 1)
scoreFromLines _ _ = 0

-- |
addLinesCleared :: Int -> Player -> Player
addLinesCleared n p =
  p
    { linesCleared = linesCleared p + n,
      score = score p + scoreFromLines (level p) n
    }

-- |
level :: Player -> Level
level p = linesCleared p `div` 10

-- | Returns a new player with the given queue.
newPlayer :: Player
newPlayer =
  Player
    { queue = [],
      canHold = True,
      held = Nothing,
      linesCleared = 0,
      score = 0
    }

-- |
factory :: Tetromino -> TFactory
factory t p = t `at` p
