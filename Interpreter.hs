module Interpreter where

import Control.Applicative

data Exp = EInt Int
         | EStr String
         | EList [Exp]
         | EOp Op
         | ETrue
         | EVar String
         | ELambda [String] Exp -- ELambda params body
         -- | ELabel String Exp -- where Exp is an ELambda
         deriving (Eq, Show)

data Op = OpPlus
        | OpMinus
        | OpMult
        | OpDiv
        | OpQuote
        | OpAtom
        | OpEq
        | OpCar
        | OpCdr
        | OpCons
        | OpCond
        deriving (Eq, Show)

type FrameEnv = [(String, Exp)]
type Env = [FrameEnv]

eval :: Env -> Exp -> Exp
eval env exp = case exp of
                 EList (EOp op:_) -> evalPrimitive env exp
                 EVar s -> unsafeLookup' env s
                 EList (EList [EVar "label", EVar s, l@(ELambda params body)]:exps) ->
                   let env' = addToAL env s l
                    in eval env' $ EList (l:exps)
                 EList (EVar f : exps) -> eval env $ EList (f':exps') -- only monic
                   where f' = eval env $ EVar f
                         exps' = map (eval env) exps
                 -- EList [EVar "label", EVar s, EList (l@(ELambda params body):exps)] ->
                 --   let bindings = zip params (map (eval env) exps)
                 --   in eval (bindings:addToAL env s l) body
                 EList (ELambda params body:exps) ->
                   let bindings = zip params (map (eval env) exps)
                   in eval (bindings:env) body
                 _ -> exp

evalPrimitive :: Env -> Exp -> Exp
evalPrimitive env exp = case exp of
                    EList [EOp op, e1, e2]
                      | op `elem` [OpPlus, OpMinus, OpMult, OpDiv] -> -- arithmetic operation
                          let EInt v1 = eval env e1
                              EInt v2 = eval env e2
                          in case op of
                             OpPlus -> EInt $ v1 + v2
                             OpMinus -> EInt $ v1 - v2
                             OpMult -> EInt $ v1 * v2
                             OpDiv -> EInt $ v1 `div` v2
                    EList [EOp OpQuote, x] -> x
                    EList [EOp OpAtom, x] -> evalAtom env $ eval env x
                    EList [EOp OpEq, x, y] -> evalEq env (eval env x) (eval env y)
                    EList [EOp OpCar, x] -> evalCar env (eval env x)
                    EList [EOp OpCdr, x] -> evalCdr env (eval env x)
                    EList [EOp OpCons, x, y] -> evalCons env (eval env x) (eval env y)
                    EList (EOp OpCond:xs) -> evalCond env xs


evalAtom :: Env -> Exp -> Exp
evalAtom env x = case x of
  EInt _ -> ETrue
  EStr _ -> ETrue
  EList [] -> ETrue
  EList _ -> EList []
  EOp _ -> ETrue
  ETrue -> ETrue

evalEq :: Env -> Exp -> Exp -> Exp
evalEq _ (EInt x) (EInt y) | x == y = ETrue
evalEq _ (EStr s1) (EStr s2) | s1 == s2 = ETrue
evalEq _ (EList []) (EList []) = ETrue
evalEq _ ETrue ETrue = ETrue
evalEq _ _ _ = EList []

evalCar :: Env -> Exp -> Exp
evalCar _ (EList (x:_)) = x

evalCdr :: Env -> Exp -> Exp
evalCdr _ (EList (_:xs)) = EList xs

evalCons :: Env -> Exp -> Exp -> Exp
evalCons _ e (EList es) = EList $ e:es

evalCond :: Env -> [Exp] -> Exp
evalCond env (EList [p,e]:es) =
  case eval env p of
    ETrue -> eval env e
    EList [] -> evalCond env es


unsafeLookup' :: Env -> String -> Exp
unsafeLookup' env s = case lookup' env s of
                        Just x -> x

lookup' :: Env -> String -> Maybe Exp
lookup' [] _ = Nothing
lookup' (env:envs) s = frameLookup env s <|> lookup' envs s

frameLookup :: FrameEnv -> String -> Maybe Exp
frameLookup fenv s = lookup s fenv

addToAL :: Env -> String -> Exp -> Env
addToAL [] s e = [[(s,e)]]
addToAL [[]] s e = [[(s,e)]]
addToAL (env:envs) s e = env':envs
  where env' = (s,e) : env'



