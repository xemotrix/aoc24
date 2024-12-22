module Day22 (run) where

import Combinators (both)
import Control.Arrow ((&&&))
import Data.Bits (xor)
import Data.Map (Map)
import Data.Map qualified as M

run :: String -> (String, String)
run = both show . (part1 &&& part2) . map read . lines

part1, part2 :: [Int] -> Int
part1 = sum . map ((!! 2000) . iterate step)
part2 = maximum . M.elems . foldl1 (M.unionWith (+)) . map changeMap

changeMap :: Int -> Map [Int] Int
changeMap = toMap . changes . zipWithDiff . map (`mod` 10) . prices
  where
    prices = (:) <*> iterate step
    zipWithDiff = map (snd &&& uncurry subtract) . (zip <*> tail)
    toMap = M.fromList . reverse . take 2000
    changes = map change . iterate tail
    change = map snd . take 4 &&& fst . (!! 3)

step :: Int -> Int
step = foldl1 (.) (map mixAndPrune [(* 2048), (`div` 32), (* 64)])
  where
    mixAndPrune f sn = (sn `xor` f sn) `mod` 16777216
