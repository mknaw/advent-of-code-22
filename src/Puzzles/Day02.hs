module Puzzles.Day02
  ( day02aSolve,
    day02bSolve,
  )
where

import qualified Data.Text as T
import Lib.Parse
import Puzzles.Puzzles
import Text.Megaparsec
import Text.Megaparsec.Char

day02aSolve :: PuzzleSolve [Round] Int
day02aSolve =
  PuzzleSolve
    { _parse = parse' (zipParser [('X', Rock), ('Y', Paper), ('Z', Scissors)]),
      _solve = day02a
    }

day02bSolve :: PuzzleSolve [(Move, Outcome)] Int
day02bSolve =
  PuzzleSolve
    { _parse = parse' (zipParser [('X', Lose), ('Y', Draw), ('Z', Win)]),
      _solve = day02b
    }

data Move = Rock | Paper | Scissors deriving (Eq)
instance Ord Move where
  compare Rock Paper = LT
  compare Paper Scissors = LT
  compare Scissors Rock = LT
  compare a b | a == b = EQ
              | otherwise = GT

type Round = (Move, Move)

data Outcome = Lose | Draw | Win

zipParser :: [(Char, a)] -> Parser a
zipParser xs = choice [ out <$ char inp | (inp, out) <- xs ]

parse' :: Parser a -> T.Text -> [(Move, a)]
parse' parseSecondColumn = parseInput $ parseRound `sepEndBy1` newline
  where
    parseRound = do
      first <- zipParser [('A', Rock), ('B', Paper), ('C', Scissors)]
      _ <- char ' '
      second <- parseSecondColumn
      return (first, second)

day02a :: [Round] -> Int
day02a = sum . map score

day02b :: [(Move, Outcome)] -> Int
day02b = sum . map (score . ((,) <$> fst <*> followStrategy))

score :: Round -> Int
score rnd = moveScore (snd rnd) + outcomeScore rnd
  where
    moveScore Rock = 1
    moveScore Paper = 2
    moveScore Scissors = 3

    outcomeScore (theirs, mine)
      | mine > theirs = 6
      | mine == theirs = 3
      | otherwise = 0

-- Could do something more elegant than this enum... like a "next biggest"
-- But can save the fancy shit for later days
followStrategy :: (Move, Outcome) -> Move
followStrategy (move, Draw) = move
followStrategy (Rock, Win) = Paper
followStrategy (Paper, Win) = Scissors
followStrategy (Scissors, Win) = Rock
followStrategy (Rock, Lose) = Scissors
followStrategy (Scissors, Lose) = Paper
followStrategy (Paper, Lose) = Rock
