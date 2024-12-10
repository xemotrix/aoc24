module Day9 (run) where

import Combinators (both)
import Control.Arrow (second, (&&&))
import Data.List (group)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Sequence (Seq (Empty, (:<|), (:|>)), breakl, fromList, (><))

run :: String -> (String, String)
run = both show . (part1 &&& part2) . parse 0

type Block = (Int, Maybe Int)

part1, part2 :: [Maybe Int] -> Int
part1 = score . catMaybes . compact [] . fromList
part2 = score . explode . defragment Empty . aggFiles
  where
    aggFiles :: [Maybe Int] -> Seq Block
    aggFiles = fromList . map (length &&& fmap head . sequence) . group
    explode = concatMap $ uncurry replicate . second (fromMaybe 0)

score :: [Int] -> Int
score = sum . zipWith (*) [0 ..]

compact :: [Maybe Int] -> Seq (Maybe Int) -> [Maybe Int]
compact acc Empty = reverse acc
compact acc (s :|> Nothing) = compact acc s
compact acc (Just a :<| s) = compact (Just a : acc) s
compact acc (Nothing :<| (s :|> Just z)) = compact (Just z : acc) s
compact acc (Nothing :<| (s :|> Nothing)) = compact acc s
compact acc (Nothing :<| Empty) = reverse acc

defragment :: Seq Block -> Seq Block -> Seq Block
defragment tailAcc Empty = tailAcc
defragment tailAcc (s :|> x@(_, Nothing)) = defragment (x :<| tailAcc) s
defragment tailAcc (s :|> x@(size, Just _)) = case breakl validHole s of
  (_, Empty) -> defragment (x :<| tailAcc) s
  (h, (holeSize, _) :<| t) ->
    let t' = if size == holeSize then t else (holeSize - size, Nothing) :<| t
     in defragment ((size, Nothing) :<| tailAcc) ((h :|> x) >< t')
  where
    validHole (hSize, Nothing) = size <= hSize
    validHole _ = False

parse :: Int -> String -> [Maybe Int]
parse _ [] = []
parse _ ['\n'] = []
parse i (c : cs) = replicate (read [c]) fileID ++ parse (i + 1) cs
  where
    fileID = if odd i then Nothing else Just (i `div` 2)
