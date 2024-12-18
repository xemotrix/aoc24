module Day18 (run) where

import Combinators (both, (.:))
import Control.Arrow ((&&&))
import Data.Function (on)
import Data.List (find, nubBy, singleton, sortBy)
import Data.Set (Set)
import Data.Set qualified as S
import Debug.Trace (traceShow)
import Utils (split)

type Point = (Int, Int)

data State = State {points :: [(Point, Int)], visited :: Set Point}

newState :: Int -> [Point] -> State
newState = State [((0, 0), 0)] . S.fromList .: take

size :: Int
size = 71

end :: Point
end = (size - 1, size - 1)

run :: String -> (String, String)
run = (part1 &&& part2) . parse

part1, part2 :: [Point] -> String
part1 = show . iterBFS . newState 1024
part2 = show . ((!!) <*> subtract 1 . (binSearch 1025 =<< length))

binSearch :: Int -> Int -> [Point] -> Int
binSearch l h _ | l == h = traceShow (l, h) l
binSearch l h bs =
  case both path (mid, mid + 1) of
    (Just _, Nothing) -> mid + 1
    (Just _, Just _) -> binSearch mid h bs
    (Nothing, _) -> binSearch l mid bs
  where
    mid = (l + h) `div` 2
    path = iterBFS . (`newState` bs)

iterBFS :: State -> Maybe Int
iterBFS State {points = []} = Nothing
iterBFS s@State {points = ps} =
  case find ((end ==) . fst) ns of
    Nothing -> iterBFS s {points = points', visited = visited'}
    Just (_, cost) -> Just cost
  where
    ns = concatMap (neigs s) ps
    points' = nubBy ((==) `on` fst) $ sortBy (compare `on` snd) ns
    visited' = foldr (S.insert . fst) (visited s) ns

neigs :: State -> (Point, Int) -> [(Point, Int)]
neigs s ((x, y), cost) = map (,cost + 1) $ filter valid neigs'
  where
    valid = and . ([inBounds, notVisited] <*>) . singleton
    inBounds (x', y') = x' >= 0 && x' < size && y' >= 0 && y' < size
    notVisited = (`S.notMember` visited s)
    neigs' = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

parse :: String -> [Point]
parse = map (both read . (head &&& last) . split ',') . lines
