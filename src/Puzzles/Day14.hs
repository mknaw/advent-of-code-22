module Puzzles.Day14
  ( day14aSolve,
    day14bSolve,
  )
where

import Control.Monad.Loops
import qualified Data.List as L
import qualified Data.List.Safe as Safe
import qualified Data.Matrix as M
import qualified Data.Text as T
import Lib.Parse
import Lib.Utils
import Puzzles.Puzzles
import Text.Megaparsec
import Text.Megaparsec.Char

day14aSolve :: PuzzleSolve [Wall] Int
day14aSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day14a
    }

day14bSolve :: PuzzleSolve [Wall] Int
day14bSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day14b
    }

parse' :: T.Text -> [Wall]
parse' = parseInput $ parseWall `sepEndBy` newline
  where
    parsePoint :: Parser Point
    parsePoint = do
      x <- parseInt
      char ','
      y <- parseInt
      return (x, y)

    parseWall :: Parser Wall
    parseWall = parsePoint `sepBy` string " -> "

type Point = (Int, Int)

type Wall = [Point]

type Grid = M.Matrix Bool

pointRange :: Point -> Point -> [Point]
pointRange (x1, y1) (x2, y2)
  | x1 == x2 = [(x1, y) | y <- [min y1 y2 .. max y1 y2]]
  | otherwise = [(x, y1) | x <- [min x1 x2 .. max x1 x2]]

placePoint :: Point -> Grid -> Grid
placePoint = M.setElem True

mkGrid :: [Wall] -> Grid
mkGrid walls = L.foldl' mkWall (M.matrix (1 + maxY) (1 + maxX) $ const False) walls'
  where
    mkWall :: Grid -> Wall -> Grid
    mkWall g = L.foldl' (flip placePoint) g . concat . (zipWith pointRange <*> tail)

    maxX = maximum . map fst . concat $ walls
    maxY = maximum . map snd . concat $ walls

    walls' = map (map (\(x, y) -> (y + 1, x + 1))) walls

propagate :: Point -> Grid -> Maybe Point
propagate p@(i, j) g = do
  candidates <-
    dropWhileM
      (\(a, b) -> M.safeGet a b g)
      [(i + 1, j), (i + 1, j - 1), (i + 1, j + 1), (i, j)]
  next <- Safe.head candidates
  if next == p then return next else propagate next g

nextGrid :: Grid -> Maybe Grid
nextGrid g = do
  let (x0, y0) = (1, 501)
  p <- propagate (x0, y0) g
  return $ placePoint p g

day14a :: [Wall] -> Int
day14a = length . iterateUntilNothing nextGrid . mkGrid

-- TODO a little slow on the full size data...
day14b :: [Wall] -> Int
day14b walls =
  length
    . iterateUntilNothing nextGrid
    . mkGrid
    . ([(0, maxY + 2), (1001, maxY + 2)] :)
    $ walls
  where
    maxY = maximum . map snd . concat $ walls
