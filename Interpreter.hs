module Interpreter where

data Exp = EInt Int
         | EStr String
         | EList [Exp]
         | EOp Op
         deriving (Eq, Show)

data Op = OpPlus
        | OpMinus
        | OpMult
        | OpDiv
        deriving (Eq, Show)

eval :: Exp -> Int
eval exp = case exp of
             EInt n -> n
             EList [EOp op, e1, e2] -> let v1 = eval e1
                                           v2 = eval e2
                                       in case op of
                                            OpPlus -> v1 + v2
                                            OpMinus -> v1 - v2
                                            OpMult -> v1 * v2
                                            OpDiv -> v1 `div` v2
