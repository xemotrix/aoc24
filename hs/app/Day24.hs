module Day24 (run) where

import Control.Arrow (second, (&&&), (***))
import Control.Monad (join)
import Data.Bits (xor, (.&.), (.|.))
import Data.Function (on)
import Data.List (find, intercalate, sort, sortBy)
import Data.Map (Map, (!))
import Data.Map qualified as M
import Data.Maybe (fromJust, isJust)
import Utils (split)

run :: String -> (String, String)
run = (part1 &&& part2) . parse

data Gate = XOR | AND | OR deriving (Eq)

data Expr
  = Iden String
  | Op Gate Expr Expr

instance Eq Expr where
  (Op op a b) == (Op op' a' b') =
    op == op' && ((a == a' && b == b') || (a == b' && b == a'))
  (Iden s) == (Iden s') = s == s'
  _ == _ = False

part1 :: (Map String Int, Map String Expr) -> String
part1 (valueMap, m) = show (toDec zsEvaluated)
  where
    zs = sortBy (compare `on` fst) $ filter ((== 'z') . head . fst) $ M.toList m
    zsExpanded = map (second (expand m)) zs
    zsEvaluated = map (eval valueMap . snd) zsExpanded
    toDec [] = 0
    toDec (b : bs) = b + 2 * toDec bs

expand :: Map String Expr -> Expr -> Expr
expand _ e@(Iden ('x' : _)) = e
expand _ e@(Iden ('y' : _)) = e
expand m (Iden s) = expand m (m ! s)
expand m (Op op a b) = Op op (expand m a) (expand m b)

eval :: Map String Int -> Expr -> Int
eval m (Iden s) = m ! s
eval m (Op AND e e') = eval m e .&. eval m e'
eval m (Op XOR e e') = eval m e `xor` eval m e'
eval m (Op OR e e') = eval m e .|. eval m e'

part2 :: (Map String Int, Map String Expr) -> String
part2 (_, m) = intercalate "," $ sort $ wrongGates m

wrongGates :: Map String Expr -> [String]
wrongGates m =
  case findWrongGate m of
    Just (target, wrong) -> a : b : wrongGates (swapGates a b)
      where
        (a, b) = (findExpr target, findExpr wrong)
        swapGates k1 k2 = M.insert k2 (m ! k1) $ M.insert k1 (m ! k2) m
        findExpr e = fst $ fromJust $ find ((== e) . snd) rev
        rev = map (second (expand m)) $ M.toList m
    Nothing -> []

findWrongGate :: Map String Expr -> Maybe (Expr, Expr)
findWrongGate m = join $ find isJust $ map (uncurry discrepancy) expVsRealPairs
  where
    zs = filter ((== 'z') . head . fst) $ M.toList m
    zsExpanded = map (second (expand m)) $ sortBy (compare `on` fst) zs
    zsEnum = zip [0 .. (length zs - 2)] zsExpanded
    expVsRealPairs = map (value *** snd) zsEnum

discrepancy :: Expr -> Expr -> Maybe (Expr, Expr)
discrepancy target@(Op top ta tb) real@(Op op a b)
  | top /= op = Just (target, real)
  | (ta == a && tb == b) || (ta == b && tb == a) = Nothing
  | (a /= ta && a /= tb) && (b == tb) = discrepancy ta a
  | (a /= ta && a /= tb) && (b == ta) = discrepancy tb a
  | otherwise = Just (target, real)
discrepancy (Iden s) (Iden s')
  | s == s' = Nothing
  | otherwise = Just (Iden s, Iden s')
discrepancy a b = Just (a, b)

(<^>), (<&>), (<|>) :: Expr -> Expr -> Expr
e <^> e' = Op XOR e e'
e <&> e' = Op AND e e'
e <|> e' = Op OR e e'

value :: Int -> Expr
value 0 = Iden "y00" <^> Iden "x00"
value n = (x <^> y) <^> carry (n - 1)
  where
    x = iden 'x' n
    y = iden 'y' n

carry :: Int -> Expr
carry 0 = iden 'x' 0 <&> iden 'y' 0
carry n = (x <&> y) <|> ((x <^> y) <&> carry (n - 1))
  where
    x = iden 'x' n
    y = iden 'y' n

iden :: Char -> Int -> Expr
iden reg n
  | n <= 9 = Iden $ reg : "0" ++ show n
  | otherwise = Iden $ reg : show n

parse :: String -> (Map String Int, Map String Expr)
parse input = (p1', gates)
  where
    ls = lines input
    (p1, p2) = second (drop 1) $ break (== "") ls
    p1' = M.fromList $ map (second (read . drop 2) . break (== ':')) p1
    p2' = map parseLine2 p2
    gates = M.fromList p2'

parseLine2 :: String -> (String, Expr)
parseLine2 l = (name, expr)
  where
    l' = split ' ' l
    name = l' !! 4
    left = Iden (head l')
    right = Iden (l' !! 2)
    expr = Op op left right
    op = case l' !! 1 of
      "XOR" -> XOR
      "OR" -> OR
      "AND" -> AND
      _ -> error "Invalid op"
