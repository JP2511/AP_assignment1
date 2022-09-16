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


{- Joins the string representations of the expressions using a string 
representation of the operation to be performed on the expressions. 
-}
joinExp :: Exp -> Exp -> String -> String
joinExp x y joiner = (showExp x) ++ joiner ++ (showExp y)


{- Joins the string representations of the expressions using a string 
representation of the operation to be performed on the expressions (joiner) and
adds parenthesis to the expression. 
-}
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
evalSimple (Div x y) = if b == 0 
  then error "Division: denominator is equal to zero."
  else div a b
  where
    a = evalSimple x
    b = evalSimple y
evalSimple (Pow x y) = if b < 0
  then error "Exponent lower than zero."
  else (-) a a + (^) a b
  where
    a = evalSimple x
    b = evalSimple y
evalSimple _         = error "Operation not possible to evaluate in expression."


-- ----------------------------------------------------------------------------
--  Problem 2

extendEnv :: VName -> Integer -> Env -> Env
extendEnv name value env = \x -> if x == name then Just value else env x


applyFullEval :: Exp -> Exp -> (Integer -> Integer -> Integer) ->
  Env -> Integer
applyFullEval x y f env  = f (evalFull x env) (evalFull y env)


evalFull :: Exp -> Env -> Integer
-- Constant
evalFull (Cst x)   _   = x
-- Addition
evalFull (Add x y) env = applyFullEval x y (+) env
-- Subtraction
evalFull (Sub x y) env = applyFullEval x y (-) env
-- Multiplication
evalFull (Mul x y) env = applyFullEval x y (*) env
-- Division
evalFull (Div x y) env = if b == 0
  then error "Division: denominator is equal to zero."
  else div a b
  where
    a = evalFull x env
    b = evalFull y env
-- Power
evalFull (Pow x y) env = if b < 0
  then error "Exponent lower than zero."
  else (-) a a + (^) a b
  where
    a = evalFull x env
    b = evalFull y env
-- Conditional
evalFull (If test yes no)       env = if (evalFull test env) /= 0 
  then evalFull yes env
  else evalFull no  env
-- Variable
evalFull (Var v)                env = f . env $ v where
  f Nothing  = error "Value constructor 'Nothing' in expression"
  f (Just x) = x
-- Equation
evalFull (Let var def body)     env = (value - value) + (evalFull body $ extendEnv var value env)
  where value = evalFull def env
-- Sum
evalFull (Sum var from to body) env = tryError where
  f x acc = (+) acc (evalFull body (extendEnv var x env))
  i = evalFull from env
  n = evalFull to   env
  errorMess = "Initial value bigger than final value"
  tryError = if i > n then error errorMess else foldr f 0 (enumFromTo i n)


-- ----------------------------------------------------------------------------
--  Problem 3

{- Evaluates the two expressions given. If any of the evaluate to an error, then
  it returns the error, otherwise it returns the evaluated expressions as
  a tuple.
-}
parseArgs :: Exp -> Exp -> Env -> Either ArithError (Integer, Integer)
parseArgs x y env = errorParser (evalErr x env) (evalErr y env) where
  errorParser (Left e)  _         = Left e
  errorParser _         (Left e)  = Left e
  errorParser (Right i) (Right j) = Right (i, j)


{- Applies an operation (function) to the two elements of the tuple if a tuple
  is given. If an error is provided, then the function simply propagates the 
  error.
-}
applyFuncErr :: Either ArithError (Integer, Integer) -> 
                (Integer -> Integer -> Integer) ->
                Either ArithError Integer
applyFuncErr (Left e)    _ = Left e
applyFuncErr (Right (a, b)) f = Right (f a b)


{- Evaluates an expression by performing an operation on two expressions that
are evaluated. If any error occurs, it is propagated. Otherwise, the result is
returned.
-}
parser :: Exp -> Exp -> (Integer -> Integer -> Integer) -> Env ->
          Either ArithError Integer
parser x y f env = applyFuncErr (parseArgs x y env) f


{- Checks if the parameter given is an ArithError, if it is, propagates it. If 
  it is not then it applies a given function to it.
-}
errorBind :: (a -> Either ArithError Integer) -> Either ArithError a -> 
              Either ArithError Integer
errorBind _ (Left e)  = Left e
errorBind f (Right a) = f a


{- Provides a more general case to the parser function. It takes two expressions
and evaluates them. If any of them, return an error, it propagates the error,
otherwise it applies a function that may or not return an error.
-}
parseAndBind :: Exp -> Exp -> Env -> 
                ((Integer, Integer) -> Either ArithError Integer) -> 
                Either ArithError Integer
parseAndBind x y env f = errorBind f $ parseArgs x y env


{- Similar to the parseAndBind, but only takes one expression.
-}
evalAndBind :: Exp -> Env -> 
                (Integer -> Either ArithError Integer) -> 
                Either ArithError Integer
evalAndBind x env f = errorBind f $ evalErr x env


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
evalErr (Div x y) env = parseAndBind x y env f where
  f (a, b) = if b == 0 then Left EDivZero else Right (div a b)
-- Power
evalErr (Pow x y) env = parseAndBind x y env f where
  f (a, b) = if b < 0 then Left ENegPower else Right ((^) a b)
-- Conditional
evalErr (If test yes no) env = evalAndBind test env f where
  f a = if a /= 0 then evalErr yes env else evalErr no env
-- Variable
evalErr (Var v) env = f . env $ v where
  f Nothing  = Left (EBadVar v)
  f (Just x) = Right x
-- Equation
evalErr (Let var def body) env = evalAndBind def env f where
  f val = evalErr body $ extendEnv var val env
-- Sum
evalErr (Sum var from to body) env = parseAndBind from to env f where
  f (f, t) = if  t < f 
    then Left (EOther "Sum -> Initial value bigger than final value") 
    else foldr g (Right 0) (enumFromTo f t) 
    where
      {- If at any point, the variable that stores the sum of the expressions
      reports the error, the sum stops and the error is propagated.  -}
      g _ (Left e)    = Left e
      g x (Right acc) = evalAndBind body (extendEnv var x env) h where
        h z = Right $ z + acc


-- ----------------------------------------------------------------------------
--  Problem 4 -- optional parts (if not attempted, leave them unmodified)


-- ------------- --
--  Problem 4.2  --
-- ------------- --


{- Determines the priority of each expression. If an addition or subtraction is 
nested on the right side of a subtraction, its priority is lower than if it 
wasn't. -}
getPriority :: (Num a) => Exp -> Bool -> a 
getPriority (Add _ _) True  = 1
getPriority (Add _ _) _     = 2
getPriority (Sub _ _) True  = 1
getPriority (Sub _ _) _     = 2
getPriority (Mul _ _) _     = 3
getPriority (Div _ _) _     = 3
getPriority (Pow _ _) _     = 4
getPriority (Cst _)   _     = 5


isSub :: Exp -> Bool
isSub (Sub _ _) = True
isSub _         = False


{- Adds parenthesis to part of an expression if that part has an operation with
  lower priority than the original expression. -}
showOnPriority :: Exp -> Exp -> Exp -> String -> String
showOnPriority outer left right joiner = l ++ joiner ++ r
  where
    leftPrio  = (getPriority outer False) > (getPriority left False)
    rightPrio = (getPriority outer False) > (getPriority right $ isSub outer)
    showWithParenthesis = addParenthesis . showCompact
    l = if leftPrio  then showWithParenthesis left  else showCompact left
    r = if rightPrio then showWithParenthesis right else showCompact right


showCompact :: Exp -> String
showCompact (Cst x)   = show x
showCompact (Add x y) = showOnPriority (Add x y) x y " + "
showCompact (Sub x y) = showOnPriority (Sub x y) x y " - "
showCompact (Mul x y) = showOnPriority (Mul x y) x y " * "
showCompact (Div x y) = showOnPriority (Div x y) x y " div "
showCompact (Pow x y) = showOnPriority (Pow x y) x y " ^ "
showCompact _         = error "Operation not possible to print in expression."


-- ------------- --
--  Problem 4.3  --
-- ------------- -- 

evalEager :: Exp -> Env -> Either ArithError Integer
evalEager = evalErr

evalLazy :: Exp -> Env -> Either ArithError Integer
evalLazy (Let var def body) env = f $ evalErr body env where
  f (Right a) = Right a
  f (Left _)  = evalErr (Let var def body) env
evalLazy expr env = evalErr expr env
