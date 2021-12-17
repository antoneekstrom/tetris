module Test where

import Action
import Control.Monad (foldM)
import Rows (Row, emptyRows, origin)
import Show
import Test.QuickCheck (Arbitrary (arbitrary), Gen, choose, elements, quickCheck, sample', vectorOf)
import Tetromino hiding (drop)
import Utilities

--------------------------------- Types ---------------------------------

-- |
data Matrix = Matrix [Row] (Maybe Tetromino)

-- | Several rows in a wrapper type, mainly for quickcheck.
newtype Rows = Rows [Row]

--------------------------------- Generators ---------------------------------

tfGen :: Gen (Position -> Tetromino)
tfGen = elements tetrominos

tGen :: Position -> Gen Tetromino
tGen pos = ($ pos) <$> tfGen

tGenFromRows :: [Row] -> Gen Tetromino
tGenFromRows rows = tGen (origin rows)

genRows :: Gen [Row]
genRows = (\(Rows rs) -> rs) <$> (arbitrary :: Gen Rows)

--------------------------------- Classes ---------------------------------

instance Arbitrary Color where
  arbitrary = elements [Cyan .. Red]

instance Arbitrary MoveAction where
  arbitrary = elements [Action.Left, Action.Right, Action.Down, Action.Rotate, Action.RotateCC]

instance Arbitrary Tetromino where
  arbitrary = tGenFromRows emptyRows

instance Arbitrary Rows where
  arbitrary = Rows <$> r'
    where
      rs = emptyRows
      n = choose (0, length rs)
      insertRows rows' rows = drop (length rows') rows ++ rows'
      toInsert n c = replicate n (replicate (length $ head rs) c)
      r n c = insertRows (toInsert n c) emptyRows
      r' = r <$> n <*> arbitrary

--------------------------------- Tests ---------------------------------

test_tetromino :: IO ()
test_tetromino = mapM_ quickCheck tetrominoProps