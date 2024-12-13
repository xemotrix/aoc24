module Day13 (run) where

import Combinators (both)
import Control.Arrow (first, second, (&&&))
import Data.Either (fromRight)
import Data.Matrix (Matrix)
import Data.Matrix qualified as M
import Data.Ratio (denominator, numerator)
import Utils (chunk, split)

run :: String -> (String, String)
run = both show . (part1 &&& part2) . parse

part1 :: [(Matrix Rational, Matrix Rational)] -> Integer
part1 = sum . map solve

part2 :: [(Matrix Rational, Matrix Rational)] -> Integer
part2 = part1 . map (second $ fmap (+ 10000000000000))

solve :: (Matrix Rational, Matrix Rational) -> Integer
solve = score . uncurry M.multStd . first inv
  where
    inv = fromRight undefined . M.inverse

score :: Matrix Rational -> Integer
score = score' . mapM ratioToInt . M.toList
  where
    score' (Just [a, b]) = a * 3 + b
    score' _ = 0
    ratioToInt n | denominator n == 1 = Just (numerator n)
    ratioToInt _ = Nothing

parse :: String -> [(Matrix Rational, Matrix Rational)]
parse = map toMatrix . chunk 3 . map parseNums . filter (/= "") . lines
  where
    parseNums =
      map (toRational . read @Integer)
        . split ','
        . filter (`elem` ',' : ['0' .. '9'])
    toMatrix = M.transpose . M.fromLists . take 2 &&& M.fromList 2 1 . last
