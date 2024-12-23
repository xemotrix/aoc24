module Main where

import Day1
import Day10
import Day11
import Day12
import Day13
import Day14
import Day15
import Day16
import Day17
import Day18
import Day19
import Day2
import Day20
import Day21
import Day22
import Day23
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9
import System.Environment (getArgs)

type Day = (Int, String -> (String, String))

days :: [Day]
days =
  zip
    [1 ..]
    [ Day1.run,
      Day2.run,
      Day3.run,
      Day4.run,
      Day5.run,
      Day6.run,
      Day7.run,
      Day8.run,
      Day9.run,
      Day10.run,
      Day11.run,
      Day12.run,
      Day13.run,
      Day14.run,
      Day15.run,
      Day16.run,
      Day17.run,
      Day18.run,
      Day19.run,
      Day20.run,
      Day21.run,
      Day22.run,
      Day23.run
    ]

main :: IO ()
main = do
  args <- getArgs
  let days' = case args of
        [] -> days
        [daynum] -> filter ((== read daynum) . fst) days
        _ -> error "usage: advent-of-code [day]"
  mapM_ runDay days'

runDay :: Day -> IO ()
runDay (daynum, dayf) = do
  input <- readInput daynum
  putStrLn $ fmtDay $ dayf input
  where
    fmtDay (part1, part2) =
      "day "
        ++ show daynum
        ++ "\n\tpart 1: "
        ++ part1
        ++ "\n\tpart 2: "
        ++ part2
    readInput n = readFile $ "../inputs/input" ++ show n ++ ".txt"
