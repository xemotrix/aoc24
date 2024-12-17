module Day17 (run) where

import Control.Arrow ((&&&), (***))
import Data.Bits (xor)
import GHC.Base (maxInt)
import Utils (split)

run :: String -> (String, String)
run = (part1 *** part2) . parse

iterF :: Int -> Int
iterF a = mod (xor (xor 3 (xor 2 (mod a 8))) (div a ((^) 2 (xor 2 (mod a 8))))) 8

runProgram :: Int -> [Int] -> [Int]
runProgram 0 out = out
runProgram n out = runProgram (n `div` 8) (iterF n : out)

part1 :: Int -> String
part1 initalA = show $ reverse $ runProgram initalA []

part2 :: [Int] -> String
part2 instructions = show $ findA (reverse instructions) 0

findA :: [Int] -> Int -> Int
findA [] acc = acc
findA (target : ts) acc = minimum $ maxInt : map (findA ts) incrAccs
  where
    next3Bits = map (id &&& ((acc * 8) +)) [0 .. 7]
    validOptions = filter ((== target) . iterF . snd) next3Bits
    incrAccs = map (((acc * 8) +) . fst) validOptions

parse :: String -> (Int, [Int])
parse = (parseA &&& parseProg) . lines
  where
    parseA = read . drop 2 . dropWhile (/= ':') . head
    parseProg = map read . split ',' . drop 2 . dropWhile (/= ':') . last

-- My input is
-- 00 Bst 4 // b <- a % 8
-- 02 Bxl 2 // b <- b xor 2
-- 04 Cdv 5 // c <- a / (2 ^ b)
-- 06 Bxl 3 // b <- b xor 3
-- 08 Bxc 3 // b <- b xor c
-- 10 Out 5 // out <- b % 8
-- 12 Adv 3 // a <- a / 8
-- 14 Jnz 0 // if a /= 0 goto 0
--
-- so basically:
-- while a /= 0
--     b = (a % 8) xor 2
--     c = a / (1 << b)
--     b = (b xor 3) xor c
--     out <- b % 8
--     a = a >> 3
--
-- factoring out registers to get a pure function of a
-- for each iteration:
--
-- iter a = (((a % 8) | 2) | 3) | (a / (1 << ((a % 8) | 2))) % 8
-- while a /= 0:
--     out <- iter a
--     a = a >> 3
