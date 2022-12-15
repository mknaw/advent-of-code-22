module Puzzles.Day15
  ( day15aSolve,
    day15bSolve,
  )
where

import Control.Applicative
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.Set as S
import qualified Data.Text as T
import Lib.Parse
import Lib.Utils
import Puzzles.Puzzles
import Text.Megaparsec
import Text.Megaparsec.Char

day15aSolve :: PuzzleSolve (Int, [Sensor]) Int
day15aSolve =
  PuzzleSolve
    { _parse = parse' 2000000,
      _solve = day15a
    }

day15bSolve :: PuzzleSolve (Int, [Sensor]) Int
day15bSolve =
  PuzzleSolve
    { _parse = parse' 4000000,
      _solve = day15b
    }

parse' :: Int -> T.Text -> (Int, [Sensor])
parse' d = parseInput $ (,) <$> parseTarget <*> (parseSensor `sepEndBy` newline)
  where
    parseTarget :: Parser Int
    parseTarget = do
      y <- optional . try $ do parseInt
      return $ M.fromMaybe d y

    parseSensor :: Parser Sensor
    parseSensor = uncurry Sensor . pairify <$> count 2 parsePoint
      where
        parsePoint :: Parser Point
        parsePoint = do
          someTill anySingle (string "x=")
          x <- parseInt
          string ", y="
          y <- parseInt
          return (x, y)

type Point = (Int, Int)

type Range = (Int, Int)

data Sensor = Sensor
  { _pos :: Point,
    _beacon :: Point
  }

-- | Manhattan distance
(<->) :: Point -> Point -> Int
(x1, y1) <-> (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

reach :: Sensor -> Int
reach sensor = _pos sensor <-> _beacon sensor

crossingY :: Int -> Sensor -> S.Set Int
crossingY t sensor = S.fromList [x - d .. x + d]
  where
    (x, y) = _pos sensor
    d = reach sensor - abs (t - y)

day15a :: (Int, [Sensor]) -> Int
day15a (t, sensors) =
  S.size
    . flip S.difference beaconsOnTarget
    . L.foldl1' S.union
    . map (crossingY t)
    $ sensors
  where
    beaconsOnTarget :: S.Set Int
    beaconsOnTarget = S.fromList . map fst . filter ((==) t . snd) . map _beacon $ sensors

getDiagonals :: Sensor -> (Range, Range)
getDiagonals sensor = ((x0 - r, x0 + r), (y0 - r, y0 + r))
  where
    (x, y) = _pos sensor
    y0 = y + x
    x0 = x - y
    r = reach sensor

getMidpoints :: [Int] -> [Int]
getMidpoints xs = M.catMaybes . zipWith singleMidpoint xs $ tail xs
  where
    singleMidpoint :: Int -> Int -> Maybe Int
    singleMidpoint a b
      | b - a == 1 = Just a
      | otherwise = Nothing

pointInSquare :: Point -> (Range, Range) -> Bool
pointInSquare (x, y) ((x0, x1), (y0, y1)) = x0 <= x && x < x1 && y0 <= y && y < y1

solveIntersection :: Int -> Int -> Point
solveIntersection x0 y0 = (x, y)
  where
    y = (y0 - x0) `div` 2
    x = x0 + y

inRange :: Point -> Sensor -> Bool
inRange p sensor = reach sensor >= p <-> _pos sensor

day15b :: (Int, [Sensor]) -> Int
day15b (bound, sensors) =
  if length candidates /= 1
    then error . show $ candidates
    else tune . head $ candidates
  where
    diags = (both . both) (`div` 2) . getDiagonals <$> sensors
    xs = getMidpoints . L.nub . L.sort . concatMap (unpairify . fst) $ diags
    ys = getMidpoints . L.nub . L.sort . concatMap (unpairify . snd) $ diags
    points = [(x, y) | x <- xs, y <- ys]
    inBounds (x, y) = x >= 0 && y >= 0 && x <= bound && y <= bound
    candidates =
      -- TODO not really sure why this is needed. Thought my weird algo would weed these out.
      filter (\p -> not . any (inRange p) $ sensors)
        . filter inBounds
        . map (uncurry solveIntersection . both (1 +) . both (2 *))
        . filter (\p -> not $ any (pointInSquare p) diags)
        $ points
    tune (x, y) = 4000000 * x + y
