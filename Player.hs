module Player where

import Matrix (Score, origin)
import System.Random (StdGen, newStdGen)
import Tetromino (Row, Tetromino (Tetromino), tetrominos)
import Utilities (At (at), Position, shuffle)
import Data.Bifunctor (Bifunctor(second))

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

hold :: [Row] -> Maybe Tetromino -> Player -> Player
hold rows (Just t) p | canHold p = p {held = Just $ t `at` origin rows}
hold _ _ p = p {held = Nothing}

unhold :: Player -> (Player, Maybe Tetromino)
unhold p =
  case held p of
    Just t -> (p {held = Nothing, canHold = False}, Just t)
    Nothing -> (p, Nothing)

-- | Returns a randomly shuffled queue with one of each tetromino.
randomQueue :: IO Queue
randomQueue =
  do
    f <- shuffle <$> newStdGen
    return (f tetrominos)

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