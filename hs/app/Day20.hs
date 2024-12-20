module Day20 (run) where

import Combinators (both)
import Control.Arrow ((&&&))
import Data.List (find)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as S
import Utils

run :: String -> (String, String)
run = both show . (run' 2 &&& run' 20) . parse

data State = State
  { pos :: Point,
    cost :: Int,
    end :: Point,
    path :: Set Point,
    visited :: Set Point,
    size :: Point
  }

run' :: Int -> State -> Int
run' n s = count (>= 100) $ cheats n bCosts
  where
    bCosts = M.fromList $ baseCosts s {pos = end s, end = pos s}

cheats :: Int -> Map Point Int -> [Int]
cheats len m = map snd $ concatMap (compCheat len m) (M.toList m)

compCheat :: Int -> Map Point Int -> (Point, Int) -> [((Point, Point), Int)]
compCheat len ps (p, c) = filter isValid $ map toCheat targets
  where
    targets = filter (isValidTarget . fst) $ M.toList ps
    isValidTarget p' = manhattan p p' <= len
    toCheat (tp, tc) = ((p, tp), c - tc - manhattan p tp)
    isValid ((_, to), c') = c' > 0 && to `M.member` ps

baseCosts :: State -> [(Point, Int)]
baseCosts s | pos s == end s = [(pos s, cost s)]
baseCosts s = (pos s, cost s) : baseCosts s''
  where
    s' = s {visited = S.insert (pos s) (visited s)}
    next = head $ filter isValid $ neigs (pos s')
    s'' = s' {pos = next, cost = cost s + 1}
    isValid x = all ($ x) [inPath, notVisited, inBounds (size s')]
    notVisited p = p `S.notMember` visited s'
    inPath p = p `S.member` path s'
    neigs (y, x) = [(y - 1, x), (y + 1, x), (y, x - 1), (y, x + 1)]

parse :: String -> State
parse input = State start 0 end path S.empty size
  where
    ls = lines input
    mat = indexMat ls
    size = (length &&& length . head) ls
    start = fst $ fromJust $ find ((== 'S') . snd) mat
    end = fst $ fromJust $ find ((== 'E') . snd) mat
    path = S.fromList $ map fst $ filter ((`elem` ['.', 'S', 'E']) . snd) mat
