module Lib.Utils
  ( applyToElem,
    bindN,
    boolToInt,
    both,
    indicesWhere,
    iterateUntilNothing,
    makeStack,
    pairify,
    pairMap,
    roundUpDiv,
    shift,
    subsets,
    trim,
    uncurry3,
    unpairify,
  )
where

import Control.Monad
import Data.Char (isSpace)
import Data.Stack

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

-- | Repeat monadic bind `n` times
bindN :: (Monad m, Integral n) => (a -> m a) -> n -> (a -> m a)
bindN f n = foldr (>=>) return (replicate (fromIntegral n) f)

-- | Apply a function yielding a `Maybe` until `isNothing`.
iterateUntilNothing :: (a -> Maybe a) -> a -> [a]
iterateUntilNothing f x =
  case f x of
    Nothing -> []
    Just y -> y : iterateUntilNothing f y

-- | Convert length 2 list to a pair tuple
pairify :: [a] -> (a, a)
pairify [x, y] = (x, y)
pairify _ = error "pairify: list must have exactly two elements"

-- | Convert a pair to a length 2 list
unpairify :: (a, a) -> [a]
unpairify (x, y) = [x, y]

-- | Apply a function to both elements in a pair
both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

-- | Like <$> for a 2-tuple
pairMap :: (a -> b) -> (a, a) -> (b, b)
pairMap f (x, y) = (f x, f y)

-- | Make a `Stack` from a list
makeStack :: [a] -> Stack a
makeStack = foldr (flip stackPush) stackNew

-- | Trim whitespace from the beginning and end of a string
trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

-- | Apply a function to the element of a list
applyToElem :: Int -> (a -> a) -> [a] -> [a]
applyToElem n f xs = before ++ [f $ head after] ++ tail after
  where
    (before, after) = splitAt n xs

-- | Get the indices of all elements that satisfy a predicate
indicesWhere :: (a -> Bool) -> [a] -> [Int]
indicesWhere f xs = map fst $ filter (f . snd) $ zip [0 ..] xs

-- | Generate all subsets of a certain size
subsets :: Int -> [a] -> [[a]]
subsets 0 _ = [[]]
subsets _ [] = []
subsets n (x : xs) = map (x :) (subsets (n - 1) xs) ++ subsets n xs

-- | Converts a curried function to a function on a triple.
uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f ~(a, b, c) = f a b c

roundUpDiv :: Int -> Int -> Int
roundUpDiv x y
  | x `mod` y == 0 = x `div` y
  | otherwise = x `div` y + 1

shift :: [a] -> [a]
shift [] = []
shift (x : xs) = xs ++ [x]
