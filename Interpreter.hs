module Interpreter where

import Control.Applicative
import Debug.Trace

data Exp = EInt Int
         | EList [Exp]
         | ETrue
         | EAtom String
         | EPrim String ([Exp] -> Exp)
         | ELambda [String] Exp -- ELambda params body

type FrameEnv = [(String, Exp)]
type Env = [FrameEnv]

showExp :: Exp -> String
showExp = undefined

evalExps :: Env -> [Exp] -> (Exp, Env)
evalExps env [] = (EList [], env)
evalExps env [exp] = eval env exp
evalExps env (exp:exps) = let (_,env') = eval env exp
  in evalExps env' exps

eval :: Env -> Exp -> (Exp, Env)
eval env exp = case exp of
                 EAtom s -> (unsafeLookup' env s, env)
                 EList [EAtom "quote", v] -> (v, env)
                 EList [EAtom "define", EAtom s, exp] ->
                   let v = fst $ eval env exp
                       env' = addToAL env s v
                       in (v, env')
                 EList (EList [EAtom "label", EAtom s, lmbd]:exps) ->
                   let env' = addToAL env s lmbd
                    in eval env' $ EList (lmbd:exps)
                 EList (EAtom "cond" : exps) -> (evalCond env exps, env)
                 EList (EAtom f : exps) -> case lookup' env f of
                   Just lmbd@(ELambda _ _) -> eval env $ EList (lmbd:exps)
                   Just prim@(EPrim _ f) -> (f params, env)
                     where params = map (fst . eval env) exps
                   _ -> undefined -- error - not a function
                 EList (ELambda params body:exps) ->
                   let bindings = zip params (map (fst . eval env) exps)
                   in eval (bindings:env) body
                 _ -> (exp, env)

primitives :: [Exp]
primitives = [EPrim "eq" eq
             ,EPrim "atom" atom
             ,EPrim "car" car
             ,EPrim "cdr" cdr
             ,EPrim "cons" cons
             ,EPrim "list" list
             ,EPrim "+"   $ unpackInt (+)
             ,EPrim "-"   $ unpackInt (-)
             ,EPrim "*"   $ unpackInt (*)
             ,EPrim "/"   $ unpackInt div
             ,EPrim "mod" $ unpackInt mod
             ,EPrim "="   $ unpackBool (==)
             ,EPrim "<"   $ unpackBool (<)
             ,EPrim ">"   $ unpackBool (>)
             ,EPrim ">="  $ unpackBool (>=)
             ,EPrim "<="  $ unpackBool (<=)
             ]

unpackInt :: (Int -> Int -> Int) -> [Exp] -> Exp
unpackInt op [EInt x, EInt y] = EInt (x `op` y)
unpackInt _ _ = undefined -- error!

unpackBool :: (Int -> Int -> Bool) -> [Exp] -> Exp
unpackBool op [EInt x, EInt y] = bool2exp $ x `op` y
unpackBool _ _ = undefined -- error!

eq :: [Exp] -> Exp
eq [EInt x, EInt y] | x == y = ETrue
eq [EAtom s1, EAtom s2] | s1 == s2 = ETrue
eq [EList [], EList []] = ETrue
eq [ETrue, ETrue] = ETrue
eq _ = undefined -- error!

atom :: [Exp] -> Exp
atom [EInt _] = ETrue
atom [EList []] = ETrue
atom [ETrue] = ETrue
atom [EAtom _] = ETrue
atom [_] = EList []
atom _ = undefined -- error!

car :: [Exp] -> Exp
car [EList (x:_)] = x
car _ = undefined -- error!

cdr :: [Exp] -> Exp
cdr [EList (_:xs)] = EList xs
cdr _ = undefined -- error!

cons :: [Exp] -> Exp
cons [x, EList xs] = EList (x:xs)
cons _ = undefined -- error!

list :: [Exp] -> Exp
list = EList -- TODO: is this it?

evalCond :: Env -> [Exp] -> Exp
evalCond env (EList [p,e]:es) =
  case fst $ eval env p of
    ETrue -> fst $ eval env e
    EList [] -> evalCond env es
evalCond _ _ = undefined -- error!

unsafeLookup' :: Env -> String -> Exp
unsafeLookup' env s = case lookup' env s of
  Just x -> x
  Nothing -> error $ "Unknown identifier " ++ s

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
