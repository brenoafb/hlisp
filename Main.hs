module Main where

import Parser
import Interpreter

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
  return $ eval [[]] e

parseAndEval' :: String -> IO (Maybe Exp)
parseAndEval' s =
  let pairMaybe = runP expP s
   in case pairMaybe of
    Nothing -> do
      putStrLn "Error parsing expression"
      return Nothing
    Just (e,_) -> do
      print e
      return . Just $ eval [[]] e
