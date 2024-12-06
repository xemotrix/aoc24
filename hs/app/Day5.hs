module Day5 (run) where

import Combinators (both, (.:))
import Control.Arrow (first, second, (&&&), (***))
import Data.List (sortBy)
import Data.Set (Set, fromList, member)
import Utils (split)

run :: String -> (String, String)
run = both show . (part1 &&& part2) . parse

part1, part2 :: (Set (Int, Int), [[Int]]) -> Int
part1 = score . uncurry filter . first (isSorted . cmpWRules)
part2 = score . uncurry sortInvalids
  where
    sortInvalids = (.) <$> sortWithRules <*> filterNotValid
    filterNotValid = filter . (not .: isSorted . cmpWRules)
    sortWithRules = map . sortBy . cmpWRules

score :: [[Int]] -> Int
score = sum . map middleElt
  where
    middleElt xs = xs !! (length xs `div` 2)

isSorted :: (a -> a -> Ordering) -> [a] -> Bool
isSorted cmp (x : y : xs)
  | cmp x y == GT = False
  | otherwise = isSorted cmp (y : xs)
isSorted _ _ = True

cmpWRules :: Set (Int, Int) -> Int -> Int -> Ordering
cmpWRules rs l r
  | (r, l) `member` rs = GT
  | (l, r) `member` rs = LT
  | otherwise = compare l r

parse :: String -> (Set (Int, Int), [[Int]])
parse = (parseRules *** parseNums) . splitParts
  where
    splitParts = second tail . break (== "") . lines
    parseRules = fromList . map (both read . second tail . break (== '|'))
    parseNums = map $ map read . split ','
