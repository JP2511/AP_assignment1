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
showExp _         = error "Operation not possible to print in expression"


-- ------------- --
--  Problem 1.2  --
-- ------------- --

applyOpOnExp :: Num a => Exp -> Exp -> (a -> a -> a)
applyOpOnExp x y f = f (evalSimple x) (evalSimple y)


evalSimple :: Exp -> Integer
evalSimple (Cst x)   = x
evalSimple (Add x y) = applyOpOnExp x y (+)
evalSimple (Sub x y) = applyOpOnExp x y (-)
evalSimple (Mul x y) = applyOpOnExp x y (*)
evalSimple (Div x y) = applyOpOnExp x y (div)
evalSimple (Pow x y) = applyOpOnExp x y (^)
evalSimple _         = error "Operation not possible to evaluate in expression"


-- ----------------------------------------------------------------------------
--  Problem 2

extendEnv :: VName -> Integer -> Env -> Env
extendEnv name value env = \x -> if x == name then value else env


evalFull :: Exp -> Env -> Integer
evalFull (If {test, yes, no})        _   = if test /= 0 then yes else no
evalFull (Var v)                     env = env v
evalFull (Let {var, def, body})      env = evalFull body (extendEnv var def env)
evalFull (Sum {var, from, to, body}) env = foldr f 0 (enumFromTo i n) where
  f x acc = (+) acc (evalFull body (extendEnv var x env))
  i = evalFull from env
  j = evalFull to   env
evalFull x                           _   = evalSimple x


-- ----------------------------------------------------------------------------
--  Problem 3

evalErr :: Exp -> Env -> Either ArithError Integer
evalErr = undefined

-- optional parts (if not attempted, leave them unmodified)

showCompact :: Exp -> String
showCompact = undefined

evalEager :: Exp -> Env -> Either ArithError Integer
evalEager = undefined

evalLazy :: Exp -> Env -> Either ArithError Integer
evalLazy = undefined
