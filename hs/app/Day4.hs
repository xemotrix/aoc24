module Day4 (run) where

import Combinators (both)
import Control.Arrow ((&&&))
import Data.List (singleton, transpose)
import Data.Maybe (mapMaybe)
import Parser (anyChar, expect, some, string, (<|>), (>>>))
import Utils (count, indexMat, (!?))

run :: String -> (String, String)
run = both show . (part1 &&& part2)

part1 :: String -> Int
part1 = sum . map countXmas . orientations
  where
    countXmas = length . expect (some xmasParser)
    xmasParser = string "XMAS" <|> (anyChar >>> xmasParser)

orientations :: String -> [String]
orientations = concat . (allOrientations <*>) . singleton . lines
  where
    allOrientations = (.) <$> [id, diagonal] <*> id : perpendiculars
    perpendiculars =
      [ transpose . reverse,
        map reverse . reverse,
        reverse . transpose
      ]
    diagonal = (++) <$> topDiags <*> botDiags
    topDiags = transpose . zipWith drop [0 ..]
    botDiags = transpose . map reverse . zipWith take [0 ..]

part2 :: String -> Int
part2 = count xmasValid . (map . getCrossChars <*> getALocs) . lines
  where
    getALocs = map fst . filter ((== 'A') . snd) . indexMat
    xmasValid = (`elem` ["MSMS", "SSMM", "MMSS", "SMSM"])

getCrossChars :: [String] -> (Int, Int) -> String
getCrossChars ls (y, x) = mapMaybe getCharAt corners
  where
    getCharAt (y', x') = (ls !? y') >>= (!? x')
    corners =
      [ (y - 1, x - 1),
        (y + 1, x - 1),
        (y - 1, x + 1),
        (y + 1, x + 1)
      ]
