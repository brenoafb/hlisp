module Interpreter where

import Control.Applicative
import Debug.Trace
import Data.List

data Exp = EInt Int
         | EList [Exp]
         | ETrue
         | EAtom String
         | EPrim String ([Exp] -> Maybe Exp)
         | ELambda [String] Exp -- ELambda params body

instance Show Exp where
  show = showExp

type FrameEnv = [(String, Exp)]
type Env = [FrameEnv]

showExp :: Exp -> String
showExp (EInt n) = show n
showExp (EList xs) = "(" ++ (unwords . map showExp $ xs) ++ ")"
showExp ETrue = "#t"
showExp (EAtom s) = s
showExp (EPrim s _) = "$primitive-" ++ s ++ "$"
showExp (ELambda params body) = "(lambda (" ++ unwords params ++ ") "
                                ++ showExp body ++ ")"


evalExps :: Env -> [Exp] -> Maybe (Exp, Env)
evalExps env [] = return (EList [], env)
evalExps env [exp] = eval env exp
evalExps env (exp:exps) = do
  (_, env') <- eval env exp
  evalExps env' exps

eval :: Env -> Exp -> Maybe (Exp, Env)
eval env exp = case exp of
                 EAtom s -> do
                   x <- lookup' env s
                   return (x, env)
                 EList [EAtom "quote", v] -> return (v, env)
                 EList [EAtom "define", EAtom s, exp] -> do
                   (v, _) <- eval env exp
                   let env' = addToAL env s v
                   return (v, env')
                 EList (EList [EAtom "label", EAtom s, lmbd]:exps) ->
                   let env' = addToAL env s lmbd
                    in eval env' $ EList (lmbd:exps)
                 EList (EAtom "cond" : exps) ->
                   (\x -> (x,env)) <$> evalCond env exps
                 EList (EAtom f : exps) -> case lookup' env f of
                   Just lmbd@(ELambda _ _) -> eval env $ EList (lmbd:exps)
                   Just prim@(EPrim _ f) -> do
                     p <- mapM (eval env) exps
                     let p' = map fst p
                     x <- f p'
                     return (x, env)
                 EList (ELambda params body:exps) -> do
                   p <- mapM (eval env) exps
                   let bindings = zip params (map fst p)
                   eval (bindings:env) body
                 _ -> return (exp, env)

primitives :: [Exp]
primitives = [EPrim "eq" eq
             ,EPrim "atom" atom
             ,EPrim "car" car
             ,EPrim "cdr" cdr
             ,EPrim "cons" cons
             ,EPrim "list" list
             ,EPrim "+"   $ packInt (+)
             ,EPrim "-"   $ packInt (-)
             ,EPrim "*"   $ packInt (*)
             ,EPrim "/"   $ packInt div
             ,EPrim "mod" $ packInt mod
             ,EPrim "="   $ packBool (==)
             ,EPrim "<"   $ packBool (<)
             ,EPrim ">"   $ packBool (>)
             ,EPrim ">="  $ packBool (>=)
             ,EPrim "<="  $ packBool (<=)
             ]

prim2env :: [Exp] -> Env
prim2env prims = [map (\p@(EPrim name _) -> (name, p)) prims]

packInt :: (Int -> Int -> Int) -> [Exp] -> Maybe Exp
packInt op [EInt x, EInt y] = return $ EInt (x `op` y)
packInt _ _ = Nothing

packBool :: (Int -> Int -> Bool) -> [Exp] -> Maybe Exp
packBool op [EInt x, EInt y] = return $ bool2exp $ x `op` y
packBool _ _ = Nothing

eq :: [Exp] -> Maybe Exp
eq [EInt x, EInt y] | x == y = return ETrue
eq [EAtom s1, EAtom s2] | s1 == s2 = return ETrue
eq [EList [], EList []] = return ETrue
eq [ETrue, ETrue] = return ETrue
eq [_, _] = return $ EList []
eq _ = Nothing

atom :: [Exp] -> Maybe Exp
atom [EInt _] = return ETrue
atom [EList []] = return ETrue
atom [ETrue] = return ETrue
atom [EAtom _] = return ETrue
atom [_] = return $ EList []
atom _ = Nothing

car :: [Exp] -> Maybe Exp
car [EList (x:_)] = return x
car _ = Nothing

cdr :: [Exp] -> Maybe Exp
cdr [EList (_:xs)] = return $ EList xs
cdr _ = Nothing

cons :: [Exp] -> Maybe Exp
cons [x, EList xs] = return $ EList (x:xs)
cons _ = Nothing

list :: [Exp] -> Maybe Exp
list = return . EList

evalCond :: Env -> [Exp] -> Maybe Exp
evalCond env (EList [p,e]:es) = do
  (v, _) <- eval env p
  case v of
    ETrue -> fst <$> eval env e
    EList [] -> evalCond env es
evalCond _ _ = Nothing -- error!

lookup' :: Env -> String -> Maybe Exp
lookup' [] _ = Nothing
lookup' (env:envs) s = frameLookup env s <|> lookup' envs s

frameLookup :: FrameEnv -> String -> Maybe Exp
frameLookup fenv s = lookup s fenv

addToAL :: Env -> String -> Exp -> Env
addToAL [] s e = [[(s,e)]]
addToAL [[]] s e = [[(s,e)]]
addToAL (env:envs) s e = env':envs
  where env' = (s,e) : env

bool2exp :: Bool -> Exp
bool2exp True = ETrue
bool2exp False = EList []
