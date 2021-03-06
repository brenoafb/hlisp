module Main where

import Prelude hiding (exp)
import Parser
import Interpreter
import Data.Maybe (fromJust)
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  envMaybe <- defaultEnv
  case envMaybe of
    Nothing -> putStrLn "Error: could not load default library."
    Just env ->
      if null args
      then repl env
      else do
        let filename = head args
        envMaybe' <- loadScript env filename
        case envMaybe' of
          Nothing -> putStrLn $ "Error: could not load script " ++ filename
          Just env' -> repl env'

repl :: Env -> IO ()
repl env = do
  putStr "> "
  line <- getLine
  case runP expP line of
    Nothing -> putStrLn "Error parsing line" >> repl env
    Just (exp,_) ->
      case eval env exp of
        Nothing -> putStrLn "Error evaluating expression" >> repl env
        Just (result, env') -> print result >> repl env'

-- defaultEnv consists of primitive operations along with a base library
defaultEnv :: IO (Maybe Env)
defaultEnv = loadScript prims "base.lisp"
  where prims = prim2env primitives

runScript :: Env -> FilePath -> IO (Maybe (Exp, Env))
runScript env path = do
  s <- readFile path
  case runP expsP s of
    Nothing -> return Nothing -- error parsing file
    Just (exps,rem) | not $ null rem -> return Nothing -- couldnt parse entire file
    Just (exps,[]) -> return $ evalExps env exps

loadScript :: Env -> FilePath -> IO (Maybe Env)
loadScript env path = do
  s <- readFile path
  case runP expsP s of
    Nothing -> return Nothing -- error parsing file
    Just (exps,_) -> return $ getNewEnv env exps

-- updates env with changes introduced by evaluating a list of expressions
getNewEnv :: Env -> [Exp] -> Maybe Env
getNewEnv env exps = do
  (_, env') <- evalExps env exps
  return env'

runRepl :: IO ()
runRepl = do
  envMaybe <- defaultEnv
  case envMaybe of
    Nothing -> putStrLn "Error loading default environment"
    Just env -> repl env
