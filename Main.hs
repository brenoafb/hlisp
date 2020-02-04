module Main where

import Parser
import Interpreter
import Data.Maybe (fromJust)

main :: IO ()
main = do
  s <- getContents
  mi <- parseAndEval' s
  case mi of
    Nothing -> putStrLn "Error interpreting expression"
    Just v -> print v

parseAndEval :: String -> Maybe Exp
parseAndEval s = do
  pair <- runP expP s
  let e = fst pair
  return $ eval defaultEnv e

parseAndEval' :: String -> IO (Maybe Exp)
parseAndEval' s =
  let pairMaybe = runP expP s
   in case pairMaybe of
    Nothing -> do
      putStrLn "Error parsing expression"
      return Nothing
    Just (e,_) -> do
      print e
      return . Just $ eval defaultEnv e

defaultEnv :: Env
defaultEnv = [
  map (\(x,y) -> (x, fst . fromJust $ runP expP y))
      [ ("null", "(lambda (x) (eq x ()))")
      , ("and",  "(lambda (x y) (cond (x (cond y #t)) (#t ())))")
      , ("not",  "(lambda (x) (cond (x ()) (#t #t)))")
      , ("append", "(lambda (x y) (cond ((null x) y) (#t (cons (car x) (append (cdr x) y)))))")
      , ("pair", unlines ["(lambda (x y)                                  ",
                          "   (cond ((and (null x) (null y)) ())          ",
                          "         ((and (not (atom x)) (not (atom y)))  ",
                          "          (cons (list (car x) (car y))         ",
                          "                (pair (cdr x) (cdr y))))))     "])
      , ("assoc", unlines ["(lambda (x y)                        ",
                           "  (cond ((eq (caar y) x) (cadar y))  ",
                           "        (#t (assoc x (cdr y)))))     "])
      , ("caar", "(lambda (x) (car (car x)))")
      , ("cadr", "(lambda (x) (car (cdr x)))")
      , ("cddr", "(lambda (x) (cdr (cdr x)))")
      , ("cdar", "(lambda (x) (cdr (car x)))")
      , ("cadar", "(lambda (x) (car (cdr (car x))))")
      ]]
