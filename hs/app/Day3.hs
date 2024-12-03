module Day3 (run) where

import Combinators (both)
import Control.Arrow ((&&&))
import Control.Monad (liftM2)
import Data.Maybe (fromJust)
import Parser (char, cond, digit, parse, some, string, (<<<), (<|>), (>>>))

run :: String -> IO (String, String)
run = return . both show . (part1 &&& part2) . parseInput
  where
    part1 = sum . map eval
    part2 = part1 . dontDo

data Instruction = Mul Int Int | Do | Dont | Noop
  deriving (Eq)

eval :: Instruction -> Int
eval (Mul a b) = a * b
eval _ = 0

dontDo :: [Instruction] -> [Instruction]
dontDo [] = []
dontDo (i@(Mul _ _) : is) = i : dontDo is
dontDo (Dont : is) = dontDo $ dropWhile (/= Do) is
dontDo (_ : is) = dontDo is

parseInput :: String -> [Instruction]
parseInput = fst . fromJust . parse parser
  where
    parser = some (do' <|> dont <|> mul <|> skip)
    do' = Do <$ string "do()"
    dont = Dont <$ string "don't()"
    mul =
      liftM2
        Mul
        (read <$> string "mul(" >>> some digit <<< char ',')
        (read <$> some digit <<< char ')')
    skip = Noop <$ cond (const True)
