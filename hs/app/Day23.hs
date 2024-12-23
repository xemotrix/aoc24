module Day23 (run) where

import Combinators (($$))
import Control.Arrow (second, (&&&))
import Data.Function (on)
import Data.List (intercalate, maximumBy, nub, singleton, sort)
import Data.Map (Map, (!))
import Data.Map qualified as M
import Data.Tuple (swap)
import Utils (combinations, split)

run :: String -> (String, String)
run = (part1 &&& part2) . parse

type Graph = Map String [String]

part1 :: Graph -> String
part1 = show . length . nub . setsOf3
  where
    setsOf3 = concatMap <$> findTri <*> candidates
    candidates = filter ((== 't') . head) . M.keys

findTri :: Graph -> String -> [[String]]
findTri graph node =
  [ sort [node, n1, n2]
    | (n1, n2) <- combinations $$ (graph ! node),
      n1 `elem` (graph ! n2)
  ]

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
