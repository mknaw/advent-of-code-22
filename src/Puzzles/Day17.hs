module Puzzles.Day17
  ( day17aSolve,
    day17bSolve,
  )
where

import Control.Lens.Getter ((^.))
import qualified Data.IntMap as M
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.Set as S
import qualified Data.Text as T
import Lib.Utils
import Linear.V2
import Puzzles.Puzzles

day17aSolve :: PuzzleSolve [Dir] Int
day17aSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day17a
    }

day17bSolve :: PuzzleSolve [Dir] Int
day17bSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day17b
    }

parse' :: T.Text -> [Dir]
parse' = fmap readDir . trim . T.unpack
  where
    readDir :: Char -> Dir
    readDir '<' = L
    readDir '>' = R
    readDir _ = error "Invalid input"

data Dir = L | R deriving (Show)

type Rock = [V2 Int]

-- TODO probably could do better still to have entries be bitmaps instead of sets!
type Chamber = M.IntMap (S.Set Int)

mkChamber :: Chamber
mkChamber = M.empty

rocks :: [Rock]
rocks =
  [ -- ####
    [V2 0 0, V2 1 0, V2 2 0, V2 3 0],
    -- .#.
    -- ###
    -- .#.
    [V2 1 0, V2 0 1, V2 1 1, V2 2 1, V2 1 2],
    -- ..#
    -- ..#
    -- ###
    [V2 0 0, V2 1 0, V2 2 0, V2 2 1, V2 2 2],
    -- #
    -- #
    -- #
    -- #
    [V2 0 0, V2 0 1, V2 0 2, V2 0 3],
    -- ##
    -- ##
    [V2 0 0, V2 1 0, V2 0 1, V2 1 1]
  ]

move :: Chamber -> V2 Int -> Rock -> Maybe Rock
move chamber offset rock = if validPosition chamber rock' then Just rock' else Nothing
  where
    rock' = map (+ offset) rock

moveJet :: Chamber -> Rock -> Dir -> Rock
moveJet chamber rock L = M.fromMaybe rock $ move chamber (V2 (-1) 0) rock
moveJet chamber rock R = M.fromMaybe rock $ move chamber (V2 1 0) rock

moveDown :: Chamber -> Rock -> Maybe Rock
moveDown chamber = move chamber (V2 0 (-1))

validPosition :: Chamber -> Rock -> Bool
validPosition chamber = all (validPosition' chamber)
  where
    validPosition' :: Chamber -> V2 Int -> Bool
    validPosition' c p
      | p ^. _x < 0 = False
      | p ^. _x > 6 = False
      | p ^. _y < 0 = False
      | otherwise = case M.lookup (p ^. _y) c of
        Nothing -> True
        Just s -> not $ (p ^. _x) `S.member` s

addRock :: Chamber -> Rock -> Chamber
addRock = foldl addPoint
  where
    addPoint :: Chamber -> V2 Int -> Chamber
    addPoint c p = M.insertWith S.union (p ^. _y) (S.singleton $ p ^. _x) c

heightOf :: Chamber -> Int
heightOf c = if null c then 0 else (+ 1) . fst $ M.findMax c

initializeRock :: Chamber -> Rock -> Rock
initializeRock c = map (+ V2 2 (heightOf c + 3))

dropRock :: Chamber -> Rock -> [Dir] -> (Rock, [Dir])
dropRock _ _ [] = error "malformed input"
dropRock chamber rock (d : ds) =
  case moveDown chamber rock' of
    Nothing -> (rock', ds)
    Just rock'' -> dropRock chamber rock'' ds
  where
    rock' = moveJet chamber rock d

propagate :: (Chamber, [Dir]) -> Rock -> (Chamber, [Dir])
propagate (chamber, dirs) rock = (chamber', dirs')
  where
    (rock', dirs') = dropRock chamber (initializeRock chamber rock) dirs
    chamber' = addRock chamber rock'

day17a :: [Dir] -> Int
day17a jets = heightOf . fst . last . take 2022 . tail $ L.scanl propagate (mkChamber, cycle jets) (cycle rocks)

day17b :: [Dir] -> Int
day17b = undefined
