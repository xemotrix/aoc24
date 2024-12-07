module Day7 (run) where

import Combinators (both, (.:))
import Control.Arrow ((&&&))
import Control.Monad (join)
import Data.Function (on)
import Data.List (find)
import Data.Maybe (isJust, mapMaybe)

run :: String -> (String, String)
run = both show . (part1 &&& part2) . parse

type Op = Int -> Int -> Int

type Equation = (Int, [Int])

part1, part2 :: [Equation] -> Int
part1 = sum . mapMaybe (eval [(*), (+)])
part2 = sum . mapMaybe (eval [(*), (+), read .: (++) `on` show])

eval :: [Op] -> Equation -> Maybe Int
eval ops = join . find isJust . (map . eval' <*> opCombis)
  where
    opCombis (_, nums) = mapM (const ops) [1 .. length nums - 1]

eval' :: Equation -> [Op] -> Maybe Int
eval' (res, x : y : ns) (op : ops) = eval' (res, x `op` y : ns) ops
eval' (res, [n]) _ | res == n = Just n
eval' _ _ = Nothing

parse :: String -> [Equation]
parse = map (parseRes &&& parseNums) . lines
  where
    parseRes = read . takeWhile (/= ':')
    parseNums = map read . words . drop 2 . dropWhile (/= ':')
