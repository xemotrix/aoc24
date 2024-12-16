module Day14 (run) where

import Combinators (both)
import Control.Arrow ((&&&))
import Data.Function (on)
import Data.List (minimumBy)
import Data.Maybe (fromMaybe)
import Parser
import Utils (combinations)

run :: String -> (String, String)
run = both show . (part1 &&& part2) . parseInput

type Point = (Int, Int)

type Robot = (Point, Point)

width, height :: Int
width = 101
height = 103

part1 :: [Robot] -> Int
part1 = score . map (iterTime 100)

score :: [Point] -> Int
score ps = product $ map length [q1, q2, q3, q4]
  where
    midx = width `div` 2
    midy = height `div` 2
    q1 = filter (\(x, y) -> x < midx && y < midy) ps
    q2 = filter (\(x, y) -> x > midx && y < midy) ps
    q3 = filter (\(x, y) -> x < midx && y > midy) ps
    q4 = filter (\(x, y) -> x > midx && y > midy) ps

iterTime :: Int -> Robot -> Point
iterTime n ((px, py), (vx, vy)) = (dx, dy)
  where
    dx = (px + (vx * n)) `mod` width
    dy = (py + (vy * n)) `mod` height

-- !! slow af
part2 :: [Robot] -> Int
part2 rbs = fst $ minimumBy (compare `on` snd) $ take (width * height) $ entropys <*> [rbs]
  where
    entropys = map aux [1 ..]
    aux n rbs' = (n, entropy $ map (iterTime n) rbs')

entropy :: [Point] -> Int
entropy = sum . map dist . filter (uncurry (/=)) . combis
  where
    combis x = combinations x x
    dist ((x, y), (x', y')) = abs (x - x') + abs (y - y')

parseInput :: String -> [Robot]
parseInput = map (expect parser) . lines

parser :: Parser ((Int, Int), (Int, Int))
parser = do
  n1 <- string "p=" >>> numParser
  n2 <- char ',' >>> numParser
  n3 <- string " v=" >>> numParser
  n4 <- char ',' >>> numParser
  return ((n1, n2), (n3, n4))
  where
    numParser :: Parser Int
    numParser = do
      sign <- fromMaybe '0' <$> optional (char '-')
      nums <- some digit
      return $ read $ sign : nums
