module Parser where

import Control.Applicative
import Control.Monad
import Data.Char
import Interpreter

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

expP :: Parser Exp
expP = foldr1 (<|>)
     [ EInt <$> intP
     , lambdaP
--     , labelP
     , EOp . str2op <$> opP
     , EStr <$> stringP
     , EVar <$> stringP'
     , ETrue <$ identP "#t"
     , EList <$> listP
     , quoteP
     ]

-- labelP :: Parser Exp
-- labelP = ELabel <$>
--   (op *> identP "label" *> wP' *> stringP' <* wP') <*> (lambdaP <* cp)
--   where op = charP '(' <* wP''
--         cp = wP'' *> charP ')'

lambdaP :: Parser Exp
lambdaP = ELambda  <$>
  (op *> identP "lambda" *> wP' *> op *> stringP' `sepBy` wP' <* cp) <*> (wP' *> expP <* cp)
  where op = charP '(' <* wP''
        cp = wP'' *> charP ')'

str2op :: String -> Op
str2op "+" = OpPlus
str2op "-" = OpMinus
str2op "*" = OpMult
str2op "/" = OpDiv
str2op "quote" = OpQuote
str2op "atom" = OpAtom
str2op "eq" = OpEq
str2op "car" = OpCar
str2op "cdr" = OpCdr
str2op "cons" = OpCons
str2op "cond" = OpCond

opP :: Parser String
opP = foldr1 (<|>) . map identP $ ["+", "-", "*", "/", "quote", "atom", "eq", "car", "cdr", "cons", "cond"]

listP :: Parser [Exp]
listP = op *> expP `sepBy` wP' <* cp
  where op = charP '(' <* wP''
        cp = wP'' *> charP ')'

sepBy :: Parser a -> Parser b -> Parser [a]
p `sepBy` sep = ((:) <$> p <*> many (sep *> p))
              <|> pure []

quoteP :: Parser Exp
quoteP = (\e -> EList (EOp OpQuote : [e])) <$> (charP '\'' *> expP)

intP :: Parser Int
intP = read <$> spanP' isDigit

identP :: String -> Parser String
identP = traverse charP

stringP :: Parser String
stringP = charP '\"' *> stringP' <* charP '\"'

stringP' :: Parser String
stringP' = spanP' $ \c -> not (isDigit c) && (isSymbol c || isAlphaNum c)

spanP :: (Char -> Bool) -> Parser String
spanP pred = Parser $ \s -> pure $ span pred s

spanP' :: (Char -> Bool) -> Parser String
spanP' pred = Parser $ \s -> let p@(x,xs) = span pred s
                              in case x of
                                   [] -> Nothing
                                   _ -> Just p

wP' :: Parser String
wP' = some (charP ' ' <|> charP '\n' <|> charP '\t')

wP'' :: Parser String
wP'' = many (charP ' ' <|> charP '\n' <|> charP '\t')

charP :: Char -> Parser Char
charP c = Parser $ \s ->
  case s of
    (c':cs) | c' == c -> Just (c,cs)
    _ -> Nothing
