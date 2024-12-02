module Utils where

import Combinators
import Control.Monad (liftM2)
import Debug.Trace (traceShow)

readInput :: Int -> IO String
readInput n = readFile inputPath
  where
    inputPath = "../inputs/input" ++ show n ++ ".txt"

inspect :: (Show a) => a -> a
inspect x = traceShow $$ x

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)

combinations :: [a] -> [b] -> [(a, b)]
combinations = liftM2 (,)

between :: (Ord a) => a -> a -> a -> Bool
between low high x = low <= x && x <= high

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p
