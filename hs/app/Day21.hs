module Day21 (run) where

import Combinators (both, (.:))
import Control.Arrow ((&&&))
import Data.MemoTrie (memoFix)

run :: String -> (String, String)
run = both show . (run' 2 &&& run' 25) . lines
  where
    run' n = sum . map (evalNum n)

evalNum :: Int -> String -> Int
evalNum n input = len * read (init input)
  where
    len = stepRec (n, concat $ step pathInNumPad input)

step :: (Char -> Char -> String) -> String -> [String]
step f input = zipWith ((++ "A") .: f) ('A' : input) input

stepRec :: (Int, String) -> Int
stepRec = memoFix stepRec'
  where
    stepRec' _ (0, input) = length input
    stepRec' rec (n, input) = sum $ map (rec . (n - 1,)) (step pathInDirPad input)

pathInPad :: (Int, Int) -> (Char -> (Int, Int)) -> Char -> Char -> String
pathInPad (hx, hy) cToXY from to =
  -- if goesLeft (xFrom < xTo) then move horizontally first, unless
  -- trouble (would go through the hole).
  if (xFrom < xTo) /= trouble then v ++ h else h ++ v
  where
    h = instrH xFrom xTo
    v = instrV yFrom yTo
    (xFrom, yFrom) = cToXY from
    (xTo, yTo) = cToXY to
    trouble = xFrom == hx && yTo == hy || xTo == hx && yFrom == hy

pathInNumPad, pathInDirPad :: Char -> Char -> String
pathInNumPad = pathInPad (0, 0) numPadCoords
pathInDirPad = pathInPad (0, 1) dirPadCoords

numPadCoords :: Char -> (Int, Int)
numPadCoords 'A' = (2, 0)
numPadCoords '0' = (1, 0)
numPadCoords '1' = (0, 1)
numPadCoords '2' = (1, 1)
numPadCoords '3' = (2, 1)
numPadCoords '4' = (0, 2)
numPadCoords '5' = (1, 2)
numPadCoords '6' = (2, 2)
numPadCoords '7' = (0, 3)
numPadCoords '8' = (1, 3)
numPadCoords '9' = (2, 3)
numPadCoords _ = error "Invalid number"

dirPadCoords :: Char -> (Int, Int)
dirPadCoords '<' = (0, 0)
dirPadCoords 'v' = (1, 0)
dirPadCoords '>' = (2, 0)
dirPadCoords '^' = (1, 1)
dirPadCoords 'A' = (2, 1)
dirPadCoords _ = error "Invalid direction"

instrV, instrH :: Int -> Int -> String
instrV = instructions '^' 'v'
instrH = instructions '>' '<'

instructions :: Char -> Char -> Int -> Int -> String
instructions incr decr from to
  | to > from = replicate (to - from) incr
  | otherwise = replicate (from - to) decr
