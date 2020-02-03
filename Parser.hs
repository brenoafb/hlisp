module Parser where

import Control.Applicative
import Control.Monad
import Data.Char
import Text.Read (readMaybe)

data Value = VInt Int
           | VStr String
           | VList [Value]
           | VOp Op
           deriving (Eq, Show)

data Op = OpPlus
        | OpMinus
        | OpMult
        | OpDiv
        deriving (Eq, Show)

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
  pa <|> pb = Parser $ \s -> runP pa s <|> runP pb s

instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = Parser $ \s -> do
    (x, s') <- runP p s
    let p' = f x
    runP p' s'  -- feels wrong

valueP :: Parser Value
valueP = foldr1 (<|>)
       [ VInt <$> intP
       , VOp . fromString <$> opP
       , VStr <$> stringP
       , VList <$> listP
       ]

fromString :: String -> Op
fromString "+" = OpPlus
fromString "-" = OpMinus
fromString "*" = OpMult
fromString "/" = OpDiv

opP :: Parser String
opP = foldr1 (<|>) . map identP $ ["+", "-", "*", "/"]

listP :: Parser [Value]
listP = charP '(' *> valueP `sepBy` wP <* charP ')'

-- listP :: Parser [String]
-- listP = charP '(' *> stringP `sepBy` wP <* charP ')'


sepBy :: Parser a -> Parser b -> Parser [a]
p `sepBy` sep = ((:) <$> p <*> many (sep *> p))
              <|> pure []


intP :: Parser Int
intP = read <$> spanP' isDigit

identP :: String -> Parser String
identP = traverse charP

stringP :: Parser String
stringP = spanP' $ \c -> not (isDigit c) && (isSymbol c || isAlphaNum c)

spanP :: (Char -> Bool) -> Parser String
spanP pred = Parser $ \s -> pure $ span pred s

spanP' :: (Char -> Bool) -> Parser String
spanP' pred = Parser $ \s -> let p@(x,xs) = span pred s
                              in case x of
                                   [] -> Nothing
                                   _ -> Just p

wP :: Parser Char
wP = charP ' '

charP :: Char -> Parser Char
charP c = Parser $ \s ->
  case s of
    (c':cs) | c' == c -> Just (c,cs)
    _ -> Nothing
