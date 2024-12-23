module Day23 (run) where

import Combinators (both, (.:))
import Control.Arrow (second, (&&&))
import Data.Function (on)
import Data.List (intercalate, maximumBy, nub, singleton, sort)
import Data.Map (Map, (!))
import Data.Map qualified as M
import Data.Tuple (swap)
import Utils (combinations, split)

run :: String -> (String, String)
run = both show . (part1 &&& part2) . parse

type Graph = Map String [String]

part1 :: Graph -> String
part1 = show . length . nub . filter startsT . setsOf3
  where
    setsOf3 = concatMap <$> (nub .: findTri) <*> M.keys
    startsT = any ((== 't') . head)

findTri :: Graph -> String -> [[String]]
findTri graph node = combine node combis
  where
    neigs = graph ! node
    combis = filter validCombi $ combinations neigs neigs
    validCombi (n1, n2) = maybe False (elem n2) (M.lookup n1 graph)
    combine n1 ns = [sort [n1, n2, n3] | (n2, n3) <- ns]

part2 :: Graph -> String
part2 = intercalate "," . sort . biggest . cliques . M.toList
  where
    cliques = map <$> maximalClique <*> map (singleton . fst)
    biggest = maximumBy (compare `on` length)

maximalClique :: [(String, [String])] -> [String] -> [String]
maximalClique [] acc = acc
maximalClique ((node, neigs) : tl) acc
  | all (`elem` neigs) acc = maximalClique tl (node : acc)
  | otherwise = maximalClique tl acc

parse :: String -> Graph
parse = toMap . andInverses . toEdges . lines
  where
    toMap = M.fromListWith (++) . map (second singleton)
    toEdges = map ((head &&& last) . split '-')
    andInverses = (++) <*> map swap
