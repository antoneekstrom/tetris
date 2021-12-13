module Player where

import Data.Bifunctor (Bifunctor (second))
import Matrix (Score, origin)
import System.Random (StdGen, newStdGen)
import Tetromino (Row, Tetromino (Tetromino), tetrominos)
import Utilities (At (at), Position, shuffle)

--------------------------------- Types ---------------------------------

-- | Queue of tetrominos.
type Queue = [Position -> Tetromino]

-- | The state of the player.
data Player = Player
  { held :: Maybe Tetromino,
    canHold :: Bool,
    queue :: Queue,
    score :: Score
  }

--------------------------------- Functions ---------------------------------

-- | Returns the next tetromino in the queue.
popQueue :: Player -> (Player, Position -> Tetromino)
popQueue p = (p {queue = tail $ queue p}, head $ queue p)

fromQueue :: [Row] -> Player -> (Player, Tetromino)
fromQueue rows p = second ($ origin rows) (popQueue p)

hold :: [Row] -> Maybe Tetromino -> Player -> (Player, Maybe Tetromino)
hold _ _ p | not $ canHold p = error "hold: can't hold"
hold rows t p = case t of
  Just t' -> (p {held = Just $ t' `at` origin rows, canHold = False}, held p)
  Nothing -> (p {canHold = False}, held p)

-- | Returns a randomly shuffled queue with one of each tetromino.
randomQueue :: IO Queue
randomQueue =
  do
    f <- shuffle <$> newStdGen
    return (f tetrominos++tetrominos++tetrominos)

-- | Returns a new player with a random queue.
randomPlayer :: IO Player
randomPlayer = newPlayer <$> randomQueue

-- | Returns a new player with the given queue.
newPlayer :: Queue -> Player
newPlayer q =
  Player
    { queue = q,
      canHold = True,
      held = Nothing,
      score = 0
    }