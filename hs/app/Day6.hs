module Day6 (run) where

import Combinators (both)
import Control.Arrow ((&&&))
import Data.List (find, nub)
import Data.Maybe (fromJust, mapMaybe)
import Data.Set (Set, fromList, insert, member, toList)
import Utils

run :: String -> (String, String)
run = both show . (part1 &&& part2) . parse

type Point = (Int, Int)

data State = State
  { size :: Point,
    guard :: Point,
    dir :: Direction,
    obst :: Set Point,
    visited :: Set (Point, Direction),
    loopAt :: Maybe Point,
    testObst :: Maybe Point
  }

data Direction = N | E | S | W deriving (Eq, Ord)

part1 :: State -> Int
part1 = length . calcVisited

part2 :: State -> Int
part2 = score . (map . addObst <*> calcVisited)
  where
    addObst s next = s {obst = insert next (obst s), testObst = Just next}
    score = length . nub . mapMaybe (loopAt . advance)

calcVisited :: State -> [Point]
calcVisited = nub . map fst . toList . visited . advance

advance :: State -> State
advance s | outOfBounds s = s
advance s | (guard s, dir s) `member` visited s = s {loopAt = testObst s}
advance s@(State _ guard dir obst visited _ _) =
  advance $ if next `member` obst then s' {dir = turn dir} else s' {guard = next}
  where
    s' = s {visited = insert (guard, dir) visited}
    next = step guard dir
    step (y, x) N = (y - 1, x)
    step (y, x) E = (y, x + 1)
    step (y, x) S = (y + 1, x)
    step (y, x) W = (y, x - 1)
    turn N = E
    turn E = S
    turn S = W
    turn W = N

outOfBounds :: State -> Bool
outOfBounds State {size = (h, w), guard = (y, x)} = y < 0 || y >= h || x < 0 || x >= w

parse :: String -> State
parse input =
  State
    { size = (length . head) &&& length $ ls,
      guard = fst . fromJust . find ((== '^') . snd) $ chars,
      dir = N,
      obst = fromList . map fst . filter ((== '#') . snd) $ chars,
      visited = fromList [],
      loopAt = Nothing,
      testObst = Nothing
    }
  where
    ls = lines input
    chars = indexMat ls
