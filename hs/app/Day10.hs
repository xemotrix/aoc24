module Day10 (run) where

import Combinators (both)
import Control.Arrow (second, (&&&))
import Data.List (nub, singleton)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Utils (indexMat)

run :: String -> (String, String)
run = both show . (part1 &&& part2) . parse

type Pos = ((Int, Int), Int)

type NeigFun = Pos -> [Pos]

part1, part2 :: ([Pos], NeigFun) -> Int
part1 (zeros, neigf) = sum $ map (length . nub . calcNines neigf) zeros
part2 (zeros, neigf) = sum $ map (length . calcNines neigf) zeros

calcNines :: NeigFun -> Pos -> [Pos]
calcNines neigf = ((++) <$> getNines <*> getRest) . neigf
  where
    getNines = nub . filter ((== 9) . snd)
    getRest = concatMap (calcNines neigf) . filter ((/= 9) . snd)

parse :: String -> ([Pos], Pos -> [Pos])
parse = (parseZeros &&& parseNeigF) . toMat
  where
    toMat = map (second (read . singleton)) . indexMat . lines
    parseZeros = filter ((== 0) . snd)
    parseNeigF = neigs . M.fromList . map (fst &&& id)

neigs :: Map (Int, Int) Pos -> Pos -> [Pos]
neigs m ((y, x), height) = filter diff1 $ mapMaybe (`M.lookup` m) coords
  where
    diff1 = (== 1) . subtract height . snd
    coords = [(y - 1, x), (y + 1, x), (y, x - 1), (y, x + 1)]
