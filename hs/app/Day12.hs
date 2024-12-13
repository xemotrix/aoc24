module Day12 (run) where

import Combinators (both)
import Control.Arrow ((&&&))
import Data.List (partition)
import Utils (indexMat)

run :: String -> (String, String)
run = both show . (part1 &&& part2) . parse

type Point = ((Int, Int), Char)

data Dir = N | S | E | W deriving (Show, Eq, Ord)

type Fence = ((Int, Int), Dir)

data Region = Region
  { points :: [Point],
    fences :: [Fence],
    letter :: Char
  }
  deriving (Show)

part1 :: [Region] -> Int
part1 = sum . map score
  where
    score = (*) <$> (length . points) <*> (length . fences)

part2 :: [Region] -> Int
part2 = sum . map score
  where
    score = (*) <$> (length . points) <*> (length . groupInSides [] . fences)

groupInSides :: [Fence] -> [Fence] -> [[Fence]]
groupInSides [] [] = []
groupInSides acc [] = [acc]
groupInSides [] (f : fs) = groupInSides [f] fs
groupInSides acc fs = case partition adj' fs of
  ([], _) -> acc : groupInSides [] fs
  (adjs, rest) -> groupInSides (adjs ++ acc) rest
  where
    adj' f = any (fenceAdj f) acc
    fenceAdj ((y, x), d) ((y', x'), d')
      | d == d' && d `elem` [N, S] = y' == y && 1 == abs (x - x')
      | d == d' && d `elem` [E, W] = x' == x && 1 == abs (y - y')
    fenceAdj _ _ = False

parse :: String -> [Region]
parse = groupInRegions Nothing . indexMat . lines

groupInRegions :: Maybe Region -> [Point] -> [Region]
groupInRegions Nothing [] = []
groupInRegions (Just reg) [] = [reg]
groupInRegions Nothing (p : ps) = groupInRegions (Just (newRegion p)) ps
groupInRegions (Just reg) ps = case partition (pBelongsToR reg) ps of
  ([], _) -> reg : groupInRegions Nothing ps
  (xs, others) -> groupInRegions (Just (foldr addPointToReg reg xs)) others

pBelongsToR :: Region -> Point -> Bool
pBelongsToR Region {letter = cr} (_, c) | c /= cr = False
pBelongsToR r p = any same' (pFences p)
  where
    same' f = any (sameFence f) (fences r)

sameFence :: Fence -> Fence -> Bool
sameFence ((y, x), N) ((y', x'), S) = x' == x && y' == y - 1
sameFence ((y, x), S) ((y', x'), N) = x' == x && y' == y + 1
sameFence ((y, x), E) ((y', x'), W) = y' == y && x' == x + 1
sameFence ((y, x), W) ((y', x'), E) = y' == y && x' == x - 1
sameFence _ _ = False

addPointToReg :: Point -> Region -> Region
addPointToReg p r = r {points = p : points r, fences = new ++ regF}
  where
    (redun, regF) = partition (isFenceRedundant pf) (fences r)
    new = filter (not . isFenceRedundant redun) pf
    pf = pFences p
    isFenceRedundant fs f = any (sameFence f) fs

pFences :: Point -> [Fence]
pFences ((y, x), _) = map ((y, x),) [N, S, W, E]

newRegion :: Point -> Region
newRegion p@(_, c) = Region {points = [p], fences = pFences p, letter = c}
