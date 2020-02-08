module Interpreter where

import Control.Applicative
import Debug.Trace

data Exp = EInt Int
         | EList [Exp]
         | ETrue
         | EAtom String
         | EPrim String [String] ([Exp] -> Exp)
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
                 -- EList (EAtom f : exps) -> eval env $ EList (f':exps)
                 --   where (f', _) = eval env $ EAtom f
                 EList (EAtom f : exps) -> (apply env' func, env)
                   where func@(EPrim _ params _) = unsafeLookup' env f
                         bindings = zip params (map (fst . eval env) exps)
                         env' = bindings:env
                 EList (ELambda params body:exps) ->
                   let bindings = zip params (map (fst . eval env) exps)
                   in eval (bindings:env) body
                 _ -> (exp, env)


apply :: Env -> Exp -> Exp
apply env (EPrim name _ f) = undefined
apply env (ELambda _ body) = undefined

-- evalPrimitive :: Env -> Exp -> Exp
-- evalPrimitive env exp = case exp of
--                     EList [EOp op, e1, e2]
--                       | op `elem` [OpPlus, OpMinus, OpMult, OpDiv, OpLt, OpGt, OpLeq, OpGeq] -> -- arithmetic operation
--                           let EInt v1 = fst $ eval env e1
--                               EInt v2 = fst $ eval env e2
--                           in case op of
--                              OpPlus -> EInt $ v1 + v2
--                              OpMinus -> EInt $ v1 - v2
--                              OpMult -> EInt $ v1 * v2
--                              OpDiv -> EInt $ v1 `div` v2
--                              OpLt -> bool2exp $ v1 < v2
--                              OpGt -> bool2exp $ v1 > v2
--                              OpLeq -> bool2exp $ v1 <= v2
--                              OpGeq -> bool2exp $ v1 >= v2
--                     EList [EOp OpAtom, x] -> evalAtom env . fst $ eval env x
--                     EList [EOp OpEq, x, y] -> evalEq env (fst $ eval env x) (fst $ eval env y)
--                     EList [EOp OpCar, x] -> evalCar env (fst $ eval env x)
--                     EList [EOp OpCdr, x] -> evalCdr env (fst $ eval env x)
--                     EList [EOp OpCons, x, y] -> evalCons env (fst $ eval env x) (fst $ eval env y)
--                     EList (EOp OpCond:xs) -> evalCond env xs
--                     EList (EOp OpList:xs) -> evalList env xs

evalAtom :: Env -> Exp -> Exp
evalAtom _ x = case x of
  EInt _ -> ETrue
  EList [] -> ETrue
  EList _ -> EList []
  ETrue -> ETrue
  EAtom _ -> ETrue
  ELambda _ _ -> EList []

evalEq :: Env -> Exp -> Exp -> Exp
evalEq _ (EInt x) (EInt y) | x == y = ETrue
evalEq _ (EAtom s1) (EAtom s2) | s1 == s2 = ETrue
evalEq _ (EList []) (EList []) = ETrue
evalEq _ ETrue ETrue = ETrue
evalEq _ _ _ = EList []

evalCar :: Env -> Exp -> Exp
evalCar _ (EList (x:_)) = x
evalCar _ e = undefined

evalCdr :: Env -> Exp -> Exp
evalCdr _ (EList (_:xs)) = EList xs

evalCons :: Env -> Exp -> Exp -> Exp
evalCons _ e (EList es) = EList $ e:es

evalCond :: Env -> [Exp] -> Exp
evalCond env (EList [p,e]:es) =
  case fst $ eval env p of
    ETrue -> fst $ eval env e
    EList [] -> evalCond env es

evalList :: Env -> [Exp] -> Exp
evalList env exps = EList $ map (fst . eval env) exps

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
