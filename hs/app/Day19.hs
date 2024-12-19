module Day19 (run) where

import Combinators (both)
import Control.Arrow (first, (&&&))
import Data.Maybe (mapMaybe)
import Data.MemoTrie (memoFix)
import Parser (Parser, parse, string)
import Utils (count, split)

run :: String -> (String, String)
run = both show . (count (> 0) &&& sum) . run' . parseInput

run' :: ([Parser String], [String]) -> [Int]
run' = uncurry map . first (memoFix . countCombis)

countCombis :: [Parser String] -> (String -> Int) -> String -> Int
countCombis _ _ "" = 1
countCombis ps rec str =
  sum $ map (rec . snd) $ mapMaybe (($ str) . parse) ps

parseInput :: String -> ([Parser String], [String])
parseInput = (parsePatterns &&& parseDesigns) . lines
  where
    parsePatterns = map string . split ',' . filter (/= ' ') . head
    parseDesigns = drop 2
