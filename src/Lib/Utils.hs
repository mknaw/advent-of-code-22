module Lib.Utils
  ( bindN,
    makeStack,
    pairMap,
  )
where

import Control.Monad
import Data.Stack

bindN :: (Monad m, Integral n) => (a -> m a) -> n -> (a -> m a)
bindN f n = foldr (>=>) return (replicate (fromIntegral n) f)

-- | Like <$> for a 2-tuple
pairMap :: (a -> b) -> (a, a) -> (b, b)
pairMap f (x, y) = (f x, f y)

-- | Make a `Stack` from a list
makeStack :: [a] -> Stack a
makeStack = foldr (flip stackPush) stackNew
