{-# LANGUAGE BangPatterns #-}

module Puzzles.Day16
  ( day16aSolve,
    day16bSolve,
  )
where

import Data.Char (isUpper)
import qualified Data.Graph.Inductive.Graph as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.Graph.Inductive.Query.BFS as G
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as M
import qualified Data.Set as S
import qualified Data.Text as T
import Lib.Parse
import Lib.Utils
import Puzzles.Puzzles
import Text.Megaparsec
import Text.Megaparsec.Char

data Valve = Valve
  { _name :: String,
    _flow :: Int,
    _tunnels :: [String]
  }
  deriving (Show)

instance Eq Valve where
  (==) a b = _name a == _name b

instance Ord Valve where
  compare a b = compare (_name a) (_name b)

day16aSolve :: PuzzleSolve [Valve] Int
day16aSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day16a
    }

day16bSolve :: PuzzleSolve [Valve] Int
day16bSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day16b
    }

parse' :: T.Text -> [Valve]
parse' = parseInput $ parseValve `sepEndBy` newline
  where
    parseValve :: Parser Valve
    parseValve = do
      string "Valve "
      name <- some upperChar
      someTill anySingle (char '=')
      flow <- parseInt
      takeWhileP (Just "") (not . isUpper)
      tunnels <- some upperChar `sepBy` string ", "
      return $ Valve name flow tunnels

type Graph = Gr String Int

type DistMap = M.Map (String, String) Int

data ValveMap = ValveMap
  { _valves :: S.Set Valve,
    _distMap :: DistMap
  }

onlyPositive :: [Valve] -> [Valve]
onlyPositive = filter (\v -> _flow v > 0)

-- TODO this is a mess
mkValveMap :: [Valve] -> ValveMap
mkValveMap valves = ValveMap (S.fromList valves') dm
  where
    names = map _name valves
    nodeToName = zip [1 ..] names
    nameToNode = M.fromList (zip names [1 ..])
    edges = concatMap (\v -> map (\t -> (nameToNode M.! _name v, nameToNode M.! t, 1)) (_tunnels v)) valves
    graph = G.mkGraph nodeToName edges
    valves' = onlyPositive valves
    nodes = (nameToNode M.! "AA" :) . map ((nameToNode M.!) . _name) $ valves'
    dm = M.mapKeys (both (M.fromJust . flip lookup nodeToName)) $ mkDistMap graph nodes

mkDistMap :: Graph -> [G.Node] -> M.Map (Int, Int) Int
mkDistMap graph nodes = M.fromList (zip pairs dists ++ zip (flipPair <$> pairs) dists)
  where
    pairs = map pairify $ subsets 2 nodes
    dists = map (\(i, j) -> length $ G.esp i j graph) pairs
    flipPair (a, b) = (b, a)

dist :: ValveMap -> Valve -> Valve -> Int
dist vm a b = _distMap vm M.! both _name (a, b)

type Path = [(Valve, Int)]

go :: ValveMap -> Int -> Valve -> S.Set Valve -> [Path]
go vm t v = go' vm t [(v, 0)]

-- TODO this is a mess
go' :: ValveMap -> Int -> Path -> S.Set Valve -> [Path]
go' vm totalDist prev rest
  | distThusFar > totalDist = [tail prev]
  | null rest' = [prev]
  | otherwise =
    concatMap (\(v, d) -> go' vm totalDist ((v, distThusFar + d) : prev) (S.fromList rest')) $
      zip rest' dists
  where
    distThusFar = snd . head $ prev
    curr = fst . head $ prev
    rest' = S.toList $ S.delete curr rest
    dists = map (dist vm curr) rest'

score :: Int -> [(Valve, Int)] -> Int
score t = sum . map (\(v, d) -> _flow v * (t - d))

day16a :: [Valve] -> Int
day16a valves = L.maximum . map (score 30) $ go vm 30 start (S.fromList valves')
  where
    valves' = onlyPositive valves
    start = head $ filter (\v -> _name v == "AA") valves
    !vm = mkValveMap valves

day16b :: [Valve] -> Int
day16b valves = error . show . length $ go vm 26 start (S.fromList valves')
  where
    valves' = onlyPositive valves
    start = head $ filter (\v -> _name v == "AA") valves
    !vm = mkValveMap valves
