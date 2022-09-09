-- This is a skeleton file for you to edit

module Arithmetic
  (
  showExp,
  evalSimple,
  extendEnv,
  evalFull,
  evalErr,
  showCompact,
  evalEager,
  evalLazy
  )

where

-- ----------------------------------------------------------------------------

import Definitions

-- ----------------------------------------------------------------------------
--  Problem 1

-- ------------- --
--  Problem 1.1  --
-- ------------- --

addParenthesis :: String -> String
addParenthesis exp = "(" ++ exp ++ ")"


joinExp :: Exp -> Exp -> String -> String
joinExp x y joiner = (showExp x) ++ joiner ++ (showExp y)


createRepExp :: Exp -> Exp -> String -> String
createRepExp x y joiner = addParenthesis (joinExp x y joiner)


showExp :: Exp -> String
showExp (Cst x)   = show x
showExp (Add x y) = createRepExp x y " + "
showExp (Sub x y) = createRepExp x y " - "
showExp (Mul x y) = createRepExp x y " * "
showExp (Div x y) = createRepExp x y " div "
showExp (Pow x y) = createRepExp x y " ^ "
showExp _         = error "Operation not possible to print in expression."


-- ------------- --
--  Problem 1.2  --
-- ------------- --

applySimEval :: Exp -> Exp -> (Integer -> Integer -> Integer) -> Integer
applySimEval x y f   = f (evalSimple x) (evalSimple y)


evalSimple :: Exp -> Integer
evalSimple (Cst x)   = x
evalSimple (Add x y) = applySimEval x y (+)
evalSimple (Sub x y) = applySimEval x y (-)
evalSimple (Mul x y) = applySimEval x y (*)
evalSimple (Div x y) = if b == 0 || a < b
  then error "Division where numerator is smaller than denominator"
  else div a b
  where
    a = evalSimple x
    b = evalSimple y
evalSimple (Pow x y) = applySimEval x y (^)
evalSimple _         = error "Operation not possible to evaluate in expression."


-- ----------------------------------------------------------------------------
--  Problem 2

extendEnv :: VName -> Integer -> Env -> Env
extendEnv name value env = \x -> if x == name then Just value else env x


applyFullEval :: Exp -> Exp -> (Integer -> Integer -> Integer) ->
  Env -> Integer
applyFullEval x y f env  = f (evalFull x env) (evalFull y env)


evalFull :: Exp -> Env -> Integer
evalFull (Cst x)   _   = x
evalFull (Add x y) env = applyFullEval x y (+) env
evalFull (Sub x y) env = applyFullEval x y (-) env
evalFull (Mul x y) env = applyFullEval x y (*) env
evalFull (Div x y) env = if b == 0 || a < b
  then error "Division where numerator is smaller than denominator"
  else div a b
  where
    a = evalFull x env
    b = evalFull y env
evalFull (Pow x y) env = applyFullEval x y (^) env
evalFull (If test yes no)       env = if (evalFull test env) /= 0 
  then evalFull yes env
  else evalFull no  env
evalFull (Var v)                env = f . env $ v where
  f Nothing  = error "Value constructor 'Nothing' in expression"
  f (Just x) = x
evalFull (Let var def body)     env = evalFull body $ extendEnv var value env
  where value = evalFull def env
evalFull (Sum var from to body) env = foldr f 0 (enumFromTo i n) where
  f x acc = (+) acc (evalFull body (extendEnv var x env))
  i = evalFull from env
  n = evalFull to   env
evalFull x _ = evalSimple x


-- ----------------------------------------------------------------------------
--  Problem 3

parseArgs :: Exp -> Exp -> Env -> Either ArithError (Integer, Integer)
parseArgs x y env = errorParser a b where
  a = evalErr x env
  b = evalErr y env
  errorParser (Left e)  _         = Left e
  errorParser _         (Left e)  = Left e
  errorParser (Right i) (Right j) = Right (i, j)


applyFuncErr :: Either ArithError (Integer, Integer) -> 
                (Integer -> Integer -> Integer) ->
                Either ArithError Integer
applyFuncErr (Left e)    _ = Left e
applyFuncErr (Right (a, b)) f = Right (f a b)


parser :: Exp -> Exp -> (Integer -> Integer -> Integer) -> Env ->
  Either ArithError Integer
parser x y f env = applyFuncErr (parseArgs x y env) f


evalErr :: Exp -> Env -> Either ArithError Integer
-- Constant
evalErr (Cst x)   _   = Right x
-- Addition
evalErr (Add x y) env = parser x y (+) env
-- Subtraction
evalErr (Sub x y) env = parser x y (-) env
-- Multiplication
evalErr (Mul x y) env = parser x y (*) env
-- Division
evalErr (Div x y) env = f $ parseArgs x y env where
  f (Right (a, b)) 
    | b == 0 = Left EDivZero
    | a < b  = Left (EOther "Division -> numerator smaller than denominator")
    | otherwise = Right (div a b) 
  f (Left e) = Left e
-- Power
evalErr (Pow x y) env = f $ parseArgs x y env where
  f (Right (a, b)) = if b < 0 then Left ENegPower else Right ((^) a b)
  f (Left e)       = Left e
-- Conditional
evalErr (If test yes no) env = f $ evalErr test env where
  f (Right a) = if a /= 0 then evalErr yes env else evalErr no env
  f (Left e)  = Left e
-- Variable
evalErr (Var v) env = f . env $ v where
  f Nothing  = Left (EBadVar v)
  f (Just x) = Right x
-- Equation
evalErr (Let var def body) env = f (evalErr def env) where
  f (Right val) = evalErr body $ extendEnv var val env
  f (Left e)    = Left e
-- Sum
evalErr (Sum var from to body) env = f (parseArgs from to env) where
  f (Right (a, b)) = foldr g (Right 0) (enumFromTo a b) where
    g _ (Left e)    = Left e
    g x (Right acc) = h (evalErr body $ extendEnv var x env) where
      h (Right z) = Right $ z + acc
      h e         = e
  f (Left e) = (Left e)


-- ----------------------------------------------------------------------------
--  Problem 4 -- optional parts (if not attempted, leave them unmodified)

showCompact :: Exp -> String
showCompact = undefined

evalEager :: Exp -> Env -> Either ArithError Integer
evalEager = undefined

evalLazy :: Exp -> Env -> Either ArithError Integer
evalLazy = undefined
