module Main where

import Day1
import Day2
import System.Environment (getArgs)
import Utils (readInput)

type Day = (Int, String -> IO (String, String))

days :: [Day]
days = [(1, Day1.run), (2, Day2.run)]

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
  res <- dayf input
  putStrLn $ fmtDay res
  where
    fmtDay (part1, part2) =
      "day "
        ++ show daynum
        ++ "\n\tpart 1: "
        ++ part1
        ++ "\n\tpart 2: "
        ++ part2
