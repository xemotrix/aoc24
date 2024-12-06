module Main where

import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import System.Environment (getArgs)

type Day = (Int, String -> (String, String))

days :: [Day]
days =
  [ (1, Day1.run),
    (2, Day2.run),
    (3, Day3.run),
    (4, Day4.run),
    (5, Day5.run),
    (6, Day6.run)
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
runDay (daynum, dayf) = readInput daynum >>= putStrLn . fmtDay . dayf
  where
    fmtDay (part1, part2) =
      "day "
        ++ show daynum
        ++ "\n\tpart 1: "
        ++ part1
        ++ "\n\tpart 2: "
        ++ part2
    readInput n = readFile $ "../inputs/input" ++ show n ++ ".txt"
