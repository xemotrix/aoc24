module Day15 (run) where

import Combinators (both)
import Control.Arrow (Arrow (first, second), (&&&))
import Data.List (find)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromJust, mapMaybe)
import Utils

run :: String -> (String, String)
run = both show . (part1 &&& part2) . parse

part1 :: Game -> Int
part1 game = score $ mkMoves game

part2 :: Game -> Int
part2 game = 0
  where
    game2 = toPart2 game

toPart2 :: Game -> Game
toPart2 (Game (ry, rx) cMap moves (h, w)) =
  Game (ry, rx * 2) newMap moves (h, w * 2)
  where
    newMap = M.fromList $ map (first (second (* 2))) $ M.toList cMap

mkMoves2 :: Game -> Map (Int, Int) Cell
mkMoves2 (Game _ cMap [] _) = cMap
mkMoves2 game@(Game robot cMap (m : ms) _) = mkMoves2 newGame
  where
    newGame = game {robot = newRobot, cMap = newMap, moves = ms}
    (newRobot, newMap) = mkMove2 robot cMap m

mkMove2 :: (Int, Int) -> Map (Int, Int) Cell -> Move -> ((Int, Int), Map (Int, Int) Cell)
mkMove2 robot@(ry, rx) cMap move = case M.lookup next cMap of
  Nothing -> (next, cMap)
  Just Box ->
    let (recR, recMap) = mkMove2 next cMap move
     in if recR /= next
          then (next, M.insert recR Box $ M.delete next recMap)
          else (robot, cMap)
  Just Wall -> (robot, cMap)
  where
    next = case move of
      R -> (ry, rx + 1)
      L -> (ry, rx - 1)
      U -> (ry - 1, rx)
      D -> (ry + 1, rx)

canMoveBox :: Game -> (Int, Int) -> Move -> Bool
canMoveBox g box@(by, bx) U = False
  where
    xd = M.lookup (by - 1, bx) (cMap g)
    xd' = M.lookup (by - 1, bx + 1) (cMap g)
    xd'' = M.lookup (by - 1, bx - 1) (cMap g)
canMoveBox _ _ _ = False

score :: Map (Int, Int) Cell -> Int
score m = sum $ map (uncurry (+) . first (* 100)) xd
  where
    xd = map fst $ filter ((== Box) . snd) $ M.toList m

mkMoves :: Game -> Map (Int, Int) Cell
mkMoves (Game _ cMap [] _) = cMap
mkMoves game@(Game robot cMap (m : ms) _) = mkMoves newGame
  where
    newGame = game {robot = newRobot, cMap = newMap, moves = ms}
    (newRobot, newMap) = mkMove robot cMap m

mkMove :: (Int, Int) -> Map (Int, Int) Cell -> Move -> ((Int, Int), Map (Int, Int) Cell)
mkMove robot@(ry, rx) cMap move = case M.lookup next cMap of
  Nothing -> (next, cMap)
  Just Box ->
    let (recR, recMap) = mkMove next cMap move
     in if recR /= next
          then (next, M.insert recR Box $ M.delete next recMap)
          else (robot, cMap)
  Just Wall -> (robot, cMap)
  where
    next = case move of
      R -> (ry, rx + 1)
      L -> (ry, rx - 1)
      U -> (ry - 1, rx)
      D -> (ry + 1, rx)

data Cell = Box | Wall deriving (Show, Eq)

data Move = L | R | U | D deriving (Show)

data Game = Game
  { robot :: (Int, Int),
    cMap :: Map (Int, Int) Cell,
    moves :: [Move],
    size :: (Int, Int)
  }
  deriving (Show)

parse :: String -> Game
parse input = Game robot cellMap moves' size
  where
    (mapa, moves) = break (== "") $ lines input
    size = (length &&& length . head) mapa
    mat = indexMat mapa
    robot = fst $ fromJust $ find ((== '@') . snd) mat
    cellMap = M.fromList $ mapMaybe toCell $ indexMat mapa
    moves' = concatMap (mapMaybe toMove) moves

toCell :: (a, Char) -> Maybe (a, Cell)
toCell (c, '#') = Just (c, Wall)
toCell (c, 'O') = Just (c, Box)
toCell _ = Nothing

toMove :: Char -> Maybe Move
toMove '>' = Just R
toMove '<' = Just L
toMove '^' = Just U
toMove 'v' = Just D
toMove _ = Nothing
