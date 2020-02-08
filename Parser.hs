module Parser where

import Control.Applicative
import Control.Monad
import Data.Char
import Interpreter

newtype Parser a = Parser { runP :: String -> Maybe (a, String) }

instance Functor Parser where
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
  p >>= f = Parser $ \s -> do
    (x, s') <- runP p s
    let p' = f x
    runP p' s'

expsP :: Parser [Exp]
expsP = expP `sepBy` nl
  where nl = some $ charP '\n'

expP :: Parser Exp
expP = foldr1 (<|>)
     [ EInt <$> intP
     , lambdaP
     -- , EOp . str2op <$> opP
     , EAtom <$> stringP'
     , ETrue <$ identP "#t"
     , EList <$> listP
     , quoteP
     ]

lambdaP :: Parser Exp
lambdaP = ELambda  <$>
  (op *> identP "lambda" *> wP' *> op *> stringP' `sepBy` wP' <* cp) <*> (wP' *> expP <* cp)
  where op = charP '(' <* wP''
        cp = wP'' *> charP ')'

listP :: Parser [Exp]
listP = op *> expP `sepBy` wP' <* cp
  where op = charP '(' <* wP''
        cp = wP'' *> charP ')'

sepBy :: Parser a -> Parser b -> Parser [a]
p `sepBy` sep = ((:) <$> p <*> many (sep *> p))
              <|> pure []

quoteP :: Parser Exp
quoteP = (\e -> EList (EAtom "quote" : [e])) <$> (charP '\'' *> expP)

intP :: Parser Int
intP = read <$> spanP' isDigit

identP :: String -> Parser String
identP = traverse charP

stringP :: Parser String
stringP = charP '\"' *> stringP' <* charP '\"'

stringP' :: Parser String
stringP' = (:) <$> first <*> rest
  where first = symbolP <|> letterP
        rest = many (digitP <|> symbolP <|> letterP)
        letterP = predP isLetter
        digitP = predP isDigit

spanP :: (Char -> Bool) -> Parser String
spanP p = Parser $ \s -> pure $ span p s

spanP' :: (Char -> Bool) -> Parser String
spanP' p = Parser $ \s -> let pair@(x,xs) = span p s
                              in case x of
                                   [] -> Nothing
                                   _ -> Just pair

wP' :: Parser String
wP' = some wP

wP'' :: Parser String
wP'' = many wP

wP :: Parser Char
wP = charP ' ' <|> charP '\n' <|> charP '\t'

predP :: (Char -> Bool) -> Parser Char
predP p = Parser $ \s ->
  case s of
    (c:cs) | p c -> Just (c,cs)
    _ -> Nothing

charP :: Char -> Parser Char
charP c = Parser $ \s ->
  case s of
    (c':cs) | c' == c -> Just (c,cs)
    _ -> Nothing

symbolP :: Parser Char
symbolP = foldr1 (<|>) $ map charP "-+=*/<>=!?*|:@#_~"
