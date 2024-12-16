module Day16 (run) where

import Combinators (both)
import Control.Arrow ((&&&))
import Data.Function (on)
import Data.List (find, minimumBy, nub, partition, sortBy)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as S
import GHC.Base (maxInt)
import Utils (indexMat)

run :: String -> (String, String)
run = both show . (part1 &&& part2) . trav . parse

type Point = (Int, Int)

type Pos = ((Point, Dir), Int)

data Game = Game
  { paths :: [[Pos]],
    end :: (Int, Int),
    path :: Set (Int, Int),
    visited :: Map Point (Dir, Int),
    ends :: [[Pos]],
    minEnd :: Int
  }

data Dir = N | S | E | W deriving (Show, Eq, Ord)

part1 :: Game -> Int
part1 = minimum . map (snd . head) . ends

part2 :: Game -> Int
part2 = length . nub . bestPath . ends
  where
    bestPath = map (fst . fst) . minimumBy (compare `on` (snd . head))

trav :: Game -> Game
trav g =
  case nexts of
    [] -> g
    _ -> trav g {paths = newPaths, ends = newEnds, visited = newVisited, minEnd = newMinEnd}
  where
    nexts = nextNodes g
    (ends', newPaths) = partition ((end g ==) . fst . fst . head) nexts
    newEnds = ends' ++ ends g
    newMinEnd = min (minEnd g) $ minimum (maxInt : map (snd . head) ends')
    newVisited = M.unionWith min posMap (visited g)
    posMap = M.fromList $ map ((\((a, b), c) -> (a, (b, c))) . head) $ paths g

nextNodes :: Game -> [[Pos]]
nextNodes g = merge (concatMap (filter ((&&) <$> isVisited . head <*> isTooBig) . neigs g) (paths g))
  where
    isVisited ((p, dir), cost) = case M.lookup p (visited g) of
      Just (d, cost') -> cost' > (cost + dirDiffCost dir d)
      Nothing -> True
    isTooBig = (< minEnd g) . snd . head
    dirDiffCost d d' | d == d' = 0
    dirDiffCost N S = 2000
    dirDiffCost S N = 2000
    dirDiffCost E W = 2000
    dirDiffCost W E = 2000
    dirDiffCost _ _ = 1000

merge :: [[Pos]] -> [[Pos]]
merge = merge' . sortBy (compare `on` head)

merge' :: [[Pos]] -> [[Pos]]
merge' ((x@(px, _) : xs) : (y@(py, _) : ys) : rest)
  | x == y = merge (((x : xs) ++ ys) : rest)
  | px == py = (x : xs) : merge' rest
  | otherwise = (x : xs) : merge' ((y : ys) : rest)
merge' other = other

neigs :: Game -> [Pos] -> [[Pos]]
neigs _ [] = []
neigs g ps@(p : _) = map (: ps) heads
  where
    heads = filter ((`S.member` path g) . fst . fst) $ neigs' p

neigs' :: Pos -> [Pos]
neigs' ((pos, d), score) = zip (map (delta pos &&& id) (opts d)) scores
  where
    scores = map (+ score) [1, 1001, 1001]
    opts N = [N, E, W]
    opts S = [S, E, W]
    opts E = [E, N, S]
    opts W = [W, N, S]
    delta (y, x) N = (y - 1, x)
    delta (y, x) S = (y + 1, x)
    delta (y, x) E = (y, x + 1)
    delta (y, x) W = (y, x - 1)

parse :: String -> Game
parse input = Game [[((start, E), 0)]] end path M.empty [] maxInt
  where
    ls = lines input
    mat = indexMat ls
    start = fst $ fromJust $ find ((== 'S') . snd) mat
    end = fst $ fromJust $ find ((== 'E') . snd) mat
    path = S.fromList $ map fst $ filter ((`elem` ['.', 'E']) . snd) mat
