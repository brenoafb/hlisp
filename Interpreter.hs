module Interpreter where

data Exp = EInt Int
         | EStr String
         | EList [Exp]
         | EOp Op
         | ETrue
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

eval :: Exp -> Exp
eval exp = case exp of
             EInt n -> exp
             EList (EOp op:_) -> evalPrimitive exp
             _ -> exp


evalPrimitive :: Exp -> Exp
evalPrimitive exp = case exp of
                    EList [EOp op, e1, e2]
                      | op `elem` [OpPlus, OpMinus, OpMult, OpDiv] -> -- arithmetic operation
                          let EInt v1 = eval e1
                              EInt v2 = eval e2
                          in case op of
                             OpPlus -> EInt $ v1 + v2
                             OpMinus -> EInt $ v1 - v2
                             OpMult -> EInt $ v1 * v2
                             OpDiv -> EInt $ v1 `div` v2
                    EList [EOp OpQuote, x] -> x
                    EList [EOp OpAtom, x] -> case eval x of
                      EInt _ -> ETrue
                      EStr _ -> ETrue
                      EList [] -> ETrue
                      EList _ -> EList []
                      EOp _ -> ETrue
                      ETrue -> ETrue
                    EList [EOp OpEq, x, y] -> evalEq (eval x) (eval y)
                    EList [EOp OpCar, x] -> evalCar (eval x)
                    EList [EOp OpCdr, x] -> evalCdr (eval x)
                    EList [EOp OpCons, x, y] -> evalCons (eval x) (eval y)
                    EList (EOp OpCond:xs) -> evalCond xs




evalEq :: Exp -> Exp -> Exp
evalEq (EInt x) (EInt y) | x == y = ETrue
evalEq (EStr s1) (EStr s2) | s1 == s2 = ETrue
evalEq (EList []) (EList []) = ETrue
evalEq ETrue ETrue = ETrue
evalEq _ _ = EList []

evalCar :: Exp -> Exp
evalCar (EList (x:_)) = x

evalCdr :: Exp -> Exp
evalCdr (EList (_:xs)) = EList xs

evalCons :: Exp -> Exp -> Exp
evalCons e (EList es) = EList $ e:es

evalCond :: [Exp] -> Exp
evalCond = undefined



