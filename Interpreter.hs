module Interpreter where

data Exp = EInt Int
         | EStr String
         | EList [Exp]
         | EOp Op
         | ETrue
         | EVar String
         | ELambda [String] Exp -- ELambda params body
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
                 EList (ELambda params body:_) -> undefined
                 EList (EOp op:_) -> evalPrimitive env exp
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




