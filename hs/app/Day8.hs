module Day8 (run) where

import Combinators (both, ($$), (.:))
import Control.Arrow (first, (&&&), (***))
import Data.Function (on)
import Data.List (groupBy, nub, sortBy)
import Utils (combinations, indexMat)

run :: String -> (String, String)
run = both show . (run' part1 &&& run' part2) . parse
  where
    run' = uncurry score .: first

type Ant = (Int, Int)

part1, part2 :: (Int, Int) -> [Ant] -> [Ant]
part1 = take 1 . drop 1 .: part2
part2 = takeWhile . inBounds

inBounds :: (Int, Int) -> Ant -> Bool
inBounds (h, w) (y, x) = y >= 0 && y < h && x >= 0 && x < w

score :: ([Ant] -> [Ant]) -> [[Ant]] -> Int
score f = length . nub . concatMap genAntinodes
  where
    genAntinodes = concatMap (antinodes f) . combis
    combis = filter (uncurry (/=)) . (combinations $$)

antinodes :: ([Ant] -> [Ant]) -> (Ant, Ant) -> [Ant]
antinodes f = (++) <$> harmonics (+) <*> harmonics (-)
  where
    harmonics op = f . (iterate <$> nextHarmonic op <*> fst)
    nextHarmonic op ((y, x), (y', x')) = op (y - y') *** op (x - x')

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
