module Day8 (run) where

import Combinators (both, (.:))
import Control.Arrow (first, (&&&), (***))
import Data.Function (on)
import Data.List (groupBy, nub, sortBy)
import Utils (combinations, indexMat)

run :: String -> (String, String)
run = both show . (part1 &&& part2) . parse

type Ant = (Int, Int)

part1, part2 :: ((Int, Int), [[Ant]]) -> Int
part1 = uncurry score . first (take 1 . drop 1 .: takeWhile . inBounds)
part2 = uncurry score . first (takeWhile . inBounds)

inBounds :: (Int, Int) -> Ant -> Bool
inBounds (h, w) (y, x) = y >= 0 && y < h && x >= 0 && x < w

score :: ([Ant] -> [Ant]) -> [[Ant]] -> Int
score nFilter ants = length $ nub $ concatMap (genAntinodes nFilter) ants

genAntinodes :: ([Ant] -> [Ant]) -> [Ant] -> [Ant]
genAntinodes nFilter ants =
  nub $ concatMap (antinodes nFilter) (combinations ants ants)

antinodes :: ([Ant] -> [Ant]) -> (Ant, Ant) -> [Ant]
antinodes _ (a, b) | a == b = []
antinodes nFilter ants = harmonics (+) ++ harmonics (-)
  where
    harmonics op = nFilter $ (iterate <$> harmonicFun op <*> fst) ants
    harmonicFun op ((y, x), (y', x')) = op (y - y') *** op (x - x')

parse :: String -> ((Int, Int), [[Ant]])
parse = (parseSize &&& parseAnts) . lines
  where
    parseSize = length . head &&& length
    parseAnts =
      map (map fst)
        . groupBy ((==) `on` snd)
        . sortBy (compare `on` snd)
        . filter ((/= '.') . snd)
        . indexMat
