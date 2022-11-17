module Main where

import Options.Applicative
import Puzzles.Map
import Puzzles.Puzzles

readDay :: ReadM Day
readDay = eitherReader $ \d -> Right . Day $ (read d :: Int)

-- TODO should allow running both?
readPart :: ReadM PuzzlePart
readPart = eitherReader f
  where
    f "a" = Right PartA
    f "b" = Right PartB
    f _ = Left "Invalid part"

problemSpecParser :: Parser PuzzleSpec
problemSpecParser =
  PuzzleSpec
    <$> option
      readDay
      ( long "day"
          <> short 'd'
          <> help "Which day to solve"
          <> metavar "INT"
      )
      <*> option
        readPart
        ( long "part"
            <> short 'p'
            <> metavar "PART"
            <> help "Run PART"
        )

main :: IO ()
main = do
  ps <- execParser $ info (problemSpecParser <**> helper) fullDesc
  input <- readInput ps
  let solved = applySolution (getPuzzleSolve ps) input
  -- TODO label like `Day XX, part A: xxxxx` ?
  putStrLn solved
  return ()
