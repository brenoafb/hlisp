module Main where

import Parser
import Interpreter
import Preprocessor
import Serializer
import Data.Maybe (fromJust)

main :: IO ()
main = do
  s <- getContents
  mi <- parseAndEval s
  case mi of
    Just v -> putStrLn $ show' v
    Nothing -> putStrLn "Error parsing expressions" >> return ()

loadScript :: Env -> FilePath -> IO Env
loadScript env p = do
  s <- readFile p
  case runP expsP s of
    Nothing ->
      putStrLn "error parsing script" >>= (const $ return env)
    Just (exps,_) -> return . snd $ evalExps env exps

parseAndEval :: String -> IO (Maybe Exp)
parseAndEval s = do
  env <- defaultEnv
  let pairMaybe = runP expsP s
  case pairMaybe of
    Just pair -> return . Just . fst . evalExps env $ fst pair
    Nothing -> return Nothing

parseAndEval' :: String -> IO (Maybe Exp)
parseAndEval' s =
  let pairMaybe = runP expsP s
   in case pairMaybe of
    Nothing -> do
      putStrLn "Error parsing expression"
      return Nothing
    Just (e,_) -> do
      print e
      env <- defaultEnv
      return . Just . fst $ evalExps env e

repl :: Env -> IO ()
repl env = do
  putStr "> "
  line <- getLine
  let p = runP expP line
  case p of
    Nothing -> do
      putStrLn "Error parsing line"
      repl env
    Just (exp,_) -> do
      let (result, env') = eval env exp
      putStrLn $ show' result
      repl env'

defaultEnv :: IO Env
defaultEnv = loadScript [[]] "base.lisp"
