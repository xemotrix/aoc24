module Day22 (run) where

import Combinators (both)
import Control.Arrow ((&&&))
import Data.Bits (xor)
import Data.Function (on)
import Data.List (maximumBy)
import Data.Map qualified as M

run :: String -> (String, String)
run = both show . (part1 &&& part2) . map read . lines

part1, part2 :: [Int] -> Int
part1 = sum . map ((!! 2000) . iterate step)
part2 = getBest . mergeMaps . map (toChangeMap . zipWithChanges . firstDigits)
  where
    firstDigits = map (`mod` 10) . ((:) <*> (take 2000 . iterate step))
    zipWithChanges = map (snd &&& uncurry subtract) . (zip <*> tail)
    toChangeMap = M.fromList . reverse . priceChanges
    mergeMaps = foldl1 (M.unionWith (+))
    getBest = snd . maximumBy (compare `on` snd) . M.toList

priceChanges :: [(Int, Int)] -> [([Int], Int)]
priceChanges nums
  | length nums < 4 = []
  | otherwise = (map snd (take 4 nums), fst (nums !! 3)) : priceChanges (tail nums)

step :: Int -> Int
step = foldl1 (.) (map mixPrune [(* 2048), (`div` 32), (* 64)])
  where
    mixPrune f sn' = (sn' `xor` f sn') `mod` 16777216
