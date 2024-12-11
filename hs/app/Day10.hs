module Day10 (run) where

import Combinators (both, (.:))
import Control.Arrow (first, second, (&&&))
import Data.List (nub, singleton)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Utils (indexMat)

run :: String -> (String, String)
run = both show . (part1 &&& part2) . parse

type Pos = ((Int, Int), Int)

part1, part2 :: (Pos -> [Pos], [Pos]) -> Int
part1 = sum . uncurry map . first (length . nub .: calcNines)
part2 = sum . uncurry map . first (length .: calcNines)

calcNines :: (Pos -> [Pos]) -> Pos -> [Pos]
calcNines neigf = ((++) <$> getNines <*> getRest) . neigf
  where
    getNines = nub . filter ((9 ==) . snd)
    getRest = concatMap (calcNines neigf) . filter ((9 /=) . snd)

neigs :: Map (Int, Int) Pos -> Pos -> [Pos]
neigs m ((y, x), height) = filter diff1 $ mapMaybe (`M.lookup` m) coords
  where
    diff1 = (height + 1 ==) . snd
    coords = [(y - 1, x), (y + 1, x), (y, x - 1), (y, x + 1)]

parse :: String -> (Pos -> [Pos], [Pos])
parse = (parseNeigF &&& parseZeros) . toMat
  where
    toMat = map (second (read . singleton)) . indexMat . lines
    parseZeros = filter ((0 ==) . snd)
    parseNeigF = neigs . M.fromList . map (fst &&& id)
