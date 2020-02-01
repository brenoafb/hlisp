module Parser where

import Control.Applicative
import Data.Char

newtype Parser a = Parser { runP :: String -> Maybe (a, String) }

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = Parser $ \s -> do
    (x,s') <- runP p s
    return (f x, s')

instance Applicative Parser where
  pure x = Parser $ \s -> Just (x,s)
  fp <*> p = Parser $ \s -> do
    (f, s') <- runP fp s
    (x, s'') <- runP p s'
    return (f x, s'')

instance Alternative Parser where
  empty = Parser $ const Nothing
  pa <|> pb = Parser $ \s ->
    let m1 = runP pa s
     in case m1 of
          Just _ -> m1
          Nothing -> runP pb s

instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = Parser $ \s -> do
    (x, s') <- runP p s
    let p' = f x
    runP p' s'  -- feels wrong

listP :: Parser [String]
listP = charP '(' *> stringP `sepBy` wP <* charP ')'


sepBy :: Parser a -> Parser b -> Parser [a]
p `sepBy` sep =   ((:) <$> p <*> many (sep *> p))
              <|> (:[]) <$> p
              <|> pure []

identP :: String -> Parser String
identP = traverse charP

stringP :: Parser String
stringP = spanP isAlphaNum

spanP :: (Char -> Bool) -> Parser String
spanP pred = Parser $ \s -> Just $ span pred s

wP :: Parser Char
wP = charP ' '

charP :: Char -> Parser Char
charP c = Parser $ \s ->
  case s of
    (c':cs) | c' == c -> Just (c,cs)
    _ -> Nothing
