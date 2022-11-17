{-# LANGUAGE GADTs #-}

module Puzzles.Puzzles
  ( applySolution,
    Day (..),
    inputPath,
    mkPuzzleSpec,
    PuzzlePart (PartA, PartB),
    PuzzleSpec (..),
    readInput,
    PuzzleSolve (..),
    SomeSolution (..),
  )
where

import qualified Data.Text as T
import qualified Data.Text.IO as T

data PuzzlePart = PartA | PartB

-- TODO should be a NAT instead of an Int?
newtype Day = Day {_d :: Int}

instance Show Day where
  show (Day day)
    | day >= 10 = show day
    | otherwise = '0' : show day

data PuzzleSpec = PuzzleSpec
  { unDay :: Day,
    unPart :: PuzzlePart
  }

mkPuzzleSpec :: Int -> PuzzlePart -> PuzzleSpec
mkPuzzleSpec day part = PuzzleSpec {unDay = Day day, unPart = part}

-- TODO both of these should check whether file exists
inputPath :: PuzzleSpec -> String
inputPath (PuzzleSpec day _) = "data/" ++ show day ++ ".txt"

-- TODO not yet used.
-- testPath :: PuzzleSpec -> String
-- testPath (PuzzleSpec day part) = "data/" ++ show day ++ show part ++ "-test.txt"

readInput :: PuzzleSpec -> IO T.Text
readInput ps = do
  T.readFile $ inputPath ps

data PuzzleSolve a b = PuzzleSolve
  { _parse :: T.Text -> a,
    _solve :: a -> b
  }

data SomeSolution where
    MkSomeSolution :: Show b => PuzzleSolve a b -> SomeSolution

applySolution :: SomeSolution -> T.Text -> String
applySolution (MkSomeSolution (PuzzleSolve parse solve)) input = show . solve $ parse input
