module Main where

import Parser
import Interpreter

main :: IO ()
main = do
  s <- getContents
  let mi = parseAndEval s
  case mi of
    Nothing -> putStrLn "error"
    Just v -> print v

parseAndEval :: String -> Maybe Exp
parseAndEval s = do
  pair <- runP expP s
  let e = fst pair
  return $ eval e

