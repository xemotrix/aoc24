{-# LANGUAGE LambdaCase #-}

module Parser where

newtype Parser a = P (String -> Maybe (a, String))

instance Functor Parser where
  fmap f (P p) = P $ \ts -> do
    (o, ts') <- p ts
    return (f o, ts')

instance Applicative Parser where
  pure x = P $ \ts -> Just (x, ts)
  (P p) <*> (P q) = P $ \ts -> do
    (f, ts') <- p ts
    (o, ts'') <- q ts'
    return (f o, ts'')

instance Monad Parser where
  return = pure
  (P p) >>= f = P $ \ts -> do
    (o, ts') <- p ts
    parse (f o) ts'

parse :: Parser o -> String -> Maybe (o, String)
parse (P f) = f

char :: Char -> Parser Char
char c = P $ \case
  (c' : cs) | c == c' -> Just (c', cs)
  _ -> Nothing

string :: String -> Parser String
string = mapM char

(<|>) :: Parser o -> Parser o -> Parser o
p <|> q = P $ \ts ->
  case parse p ts of
    Just x -> Just x
    Nothing -> parse q ts

(>>>) :: Parser o -> Parser o' -> Parser o'
p >>> q = do
  _ <- p
  q

(<<<) :: Parser o -> Parser o' -> Parser o
p <<< q = do
  l <- p
  _ <- q
  return l

some :: Parser o -> Parser [o]
some p =
  ( do
      x <- p
      xs <- some p
      return (x : xs)
  )
    <|> return []

dropUntil :: Parser a -> Parser a
dropUntil p = p <|> (cond (const True) >>> dropUntil p)

cond :: (Char -> Bool) -> Parser Char
cond f = P $ \ts -> do
  case ts of
    (t : ts') | f t -> Just (t, ts')
    _ -> Nothing

digit :: Parser Char
digit = cond (`elem` ['0' .. '9'])
