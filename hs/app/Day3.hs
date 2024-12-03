module Day3 (run) where

import Combinators (both, (.:))
import Control.Arrow ((&&&))
import Control.Monad (void)
import Parser

run :: String -> (String, String)
run = both show . (run' p1 &&& run' p2)
  where
    run' = sum .: expect . some
    p1 = mul <|> (anyChar >>> p1)
    p2 = mul <|> ((dontDo <|> void anyChar) >>> p2)

mul :: Parser Int
mul = (*) <$> x <*> y
  where
    x = read <$> string "mul(" >>> some digit <<< char ','
    y = read <$> some digit <<< char ')'

dontDo :: Parser ()
dontDo = void $ string "don't()" >>> dropUntil (string "do()")
