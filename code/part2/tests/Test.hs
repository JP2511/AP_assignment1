-- Very rudimentary test of Arithmetic. Feel free to replace completely

import Definitions
import Arithmetic

import Test.Tasty
import Test.Tasty.HUnit

{- Complex Expression which we couldn't fit inside of a testCase because where
  doesn't work inside of a testCase
-}
divExp :: Exp
divExp = Div (Cst 1) (Cst 2)

mulExp :: Exp
mulExp = Mul (Add (Cst 3) (Cst 4)) (Sub (Cst 6) (Cst 5))

complexExp :: Exp
complexExp = Pow divExp mulExp

complexShowResult :: String
complexShowResult = "((1 div 2) ^ ((3 + 4) * (6 - 5)))"


-- ----------------------------------------------------------------------------
--  Tests for Part 1

testShowExp = testGroup "Unit Tests for showExp"
  [
    testCase "Printing a single constant value" $
      showExp (Cst 2) @?= "2",

    testCase "Printing an addition of constants" $
      showExp (Add (Cst 1) (Cst 2)) @?= "(1 + 2)",
    
    testCase "Printing a subtraction of constants" $
      showExp (Sub (Cst 1) (Cst 2)) @?= "(1 - 2)",
    
    testCase "Printing a multiplication of constants" $
      showExp (Mul (Cst 1) (Cst 2)) @?= "(1 * 2)",

    testCase "Printing a division of constants" $
      showExp (Div (Cst 1) (Cst 2)) @?= "(1 div 2)",
    
    testCase "Printing a exponentiation of constants" $
      showExp (Pow (Cst 1) (Cst 2)) @?= "(1 ^ 2)",

    testCase "Printing a more complex expression" $
      showExp complexExp @?= complexShowResult 
  ]


testEvalSimple = testGroup "Unit Tests for evalSimple"
  [
    testCase "Evaluating a simple constant" $
      evalSimple (Cst 2) @?= 2,
    
    testCase "Evaluating an addition between constants" $
      evalSimple (Add (Cst 1) (Cst 2)) @?= 3,
    
    testCase "Evaluating a subtraction between constants" $
      evalSimple (Sub (Cst 1) (Cst 2)) @?= (-1),
    
    testCase "Evaluating a multiplication between constants" $
      evalSimple (Mul (Cst 2) (Cst 3)) @?= 6,
    
    testCase "Evaluating a division between constants" $
      evalSimple (Div (Cst 10) (Cst 5)) @?= 2,
    
    testCase "Evaluating an exponentiation between constants" $
      evalSimple (Pow (Cst 2) (Cst 3)) @?= 8,
    
    testCase "Evaluating a more complex expression" $
      evalSimple complexExp @?= 0
  ]


-- ----------------------------------------------------------------------------
--  Tests for Part 2


testExtendEnv = testGroup "Unit Tests for extendEnv"
  [
    testCase "Extending an empty environment and fetching new variable" $
      (extendEnv "x" 3 initEnv) "x" @?= Just 3,
    
    testCase "Extending an empty environment and checking a non variable" $
      (extendEnv "x" 3 initEnv) "y" @?= Nothing,

    testCase "Extending a non-empty environment and fetching new variable" $
      (extendEnv "y" 4 (extendEnv "x" 3 initEnv)) "y" @?= Just 4,
    
    testCase "Extending a non-empty environmnet and fetching old variable" $
      (extendEnv "y" 4 (extendEnv "x" 3 initEnv)) "x" @?= Just 3
  ]


-- Environment with multiple variables stored
complexEnv = \y -> if y == "y" then Just 4 else f y where
  f = \x -> if x == "x" then Just 2 else initEnv x

testEvalFull = testGroup "Unit Tests for evalFull"
  [
    testCase "Evaluating a simple constant" $
      evalFull (Cst 2) initEnv @?= 2,
    
    testCase "Evaluating an addition between constants" $
      evalFull (Add (Cst 1) (Cst 2)) initEnv @?= 3,
    
    testCase "Evaluating a subtraction between constants" $
      evalFull (Sub (Cst 1) (Cst 2)) initEnv @?= (-1),
    
    testCase "Evaluating a multiplication between constants" $
      evalFull (Mul (Cst 2) (Cst 3)) initEnv @?= 6,
    
    testCase "Evaluating a division between constants" $
      evalFull (Div (Cst 10) (Cst 5)) initEnv @?= 2,
    
    testCase "Evaluating an exponentiation between constants" $
      evalFull (Pow (Cst 2) (Cst 3)) initEnv @?= 8,
    
    testCase "Evaluating a more complex expression" $
      evalFull (Pow divExp mulExp) initEnv @?= 0,
    
    testCase "Evaluating a conditional of constants when condition is not 0" $
      evalFull (If (Cst 2) (Cst 3) undefined) initEnv @?= 3,

    testCase "Evaluating a conditional of constants when condition is 0" $
      evalFull (If (Cst 0) undefined (Cst 3)) initEnv @?= 3,
    
    testCase "Evaluating a variable in an environment with one variable" $
      evalFull (Var "x") (\x -> if x == "x" then Just 2 else initEnv x) @?= 2,

    testCase "Evaluating a variable in an environment with multiple variables" $
      evalFull (Var "x") complexEnv @?= 2,
    
    testCase "Evaluating a simple let statement" $
      evalFull (Let "x" (Cst 4) (Var "x")) initEnv @?= 4,
    
    testCase "Evaluating a more complex let statement" $
      evalFull (Let "x" complexExp (Add (Var "x") (Cst 3))) initEnv @?= 3,
    
    testCase "Evaluating a simple sum" $
      evalFull (Sum "x" (Cst 1) (Cst 5) (Var "x")) initEnv @?= 15,
    
    testCase "Evaluating a more complex sum" $
      evalFull (Sum "x" (Cst 1) (Cst 5) (Mul (Cst 2) (Var "x"))) initEnv @?= 30
  ]


-- ----------------------------------------------------------------------------
--  Tests for Part 3

{- Creates a sum expression with a given starting value -}
createSumExp :: Integer -> Exp
createSumExp x = Sum "x" (Cst x) (Cst 4) (Mul (Cst 10) (Var "x"))

sumError :: Either ArithError Integer
sumError = Left (EOther "Sum -> Initial value bigger than final value")


testEvalErr = testGroup "Unit tests for evalErr"
  [ 
    testCase "Evaluating: Constant expression" $
      evalErr (Cst 5) initEnv @?= Right 5 ,
    
    testCase "Evaluating: Division by 0" $
      evalErr (Div (Cst 5) (Cst 0)) initEnv @?= Left EDivZero,
    
    testCase "Evaluating: Division by a non-zero value" $
      evalErr (Div (Cst 5) (Cst 2)) initEnv @?= Right 2,
    
    testCase "Evaluating: Division where denominater is larger than nominater" $
      evalErr (Div (Cst 5) (Cst 6)) initEnv @?= Right 0,
    
    testCase "Evaluating: Multiplication" $
      evalErr (Mul (Cst 5) (Cst 4)) initEnv @?= Right 20,
    
    testCase "Evaluating: Subtraction" $
      evalErr (Sub (Cst 79) (Cst 80)) initEnv @?= Right (-1),
    
    testCase "Evaluating: Subtraction 2" $
      evalErr (Add (Cst 79) (Cst 21)) initEnv @?= Right 100,

    testCase "Evaluating: Exponential" $
      evalErr (Pow (Cst 3) (Cst 3)) initEnv @?= Right 27,

    testCase "Evaluating: Negative Exponential" $
      evalErr (Pow (Cst 79) (Cst (-2))) initEnv @?= Left ENegPower,
    
    testCase "Evaluating: If statment, condition == 0" $
      evalErr (If (Cst 0) (Cst 3) (Cst 2)) initEnv @?= Right 2,

    testCase "Evaluating: If statment, condition /= 0" $
      evalErr (If (Cst 100) (Cst 3) (Cst 2)) initEnv @?= Right 3,

    testCase "Evaluating: Sum" $
      evalErr (createSumExp 1) initEnv @?= Right 100,

    testCase "Evaluating: Sum where 'Start value' larger than 'End value'" $
      evalErr (createSumExp 6) initEnv @?= sumError,
    
    testCase "Evaluating: Let where the variable is divided by 0" $
      evalErr (Let "x" (Div (Cst 4) (Cst 0)) (Cst 5)) initEnv @?= Left EDivZero,

    testCase "Evaluating: Let" $
      evalErr (Let "x" (Cst 4) (Add (Cst 5) (Var "x"))) initEnv @?= Right 9,

    testCase "Evaluating: Var not in enviroment" $
      evalErr (Var "x") initEnv @?= Left (EBadVar "x"),

    testCase "Evaluating: Var in the enviroment" $
      evalErr (Var "x") (extendEnv "x" 4 initEnv) @?= Right 4,
      
    testCase "Evaluating: EDivZero error raised to the power of 0" $
      evalErr (Pow (Div (Cst 4) (Cst 0)) (Cst 0)) initEnv @?= Left EDivZero,
    
    testCase "Evaluating: 5 raised power of 0" $
      evalErr (Pow (Div (Cst 5) (Cst 1)) (Cst 0)) initEnv @?= Right 1
  ]


-- ----------------------------------------------------------------------------
--  Tests for Part 4


-- ----------------------- --
--  Tests for Problem 4.2  --
-- ----------------------- --

addWithEqPriority :: Exp
addWithEqPriority = (Add (Add (Cst 1) (Cst 2)) (Sub (Cst 3) (Cst 4)))

addWithMixPriority :: Exp
addWithMixPriority = (Add (Add (Cst 1) (Cst 2)) (Div (Cst 3) (Cst 4)))

addWithHiPriority :: Exp
addWithHiPriority = (Add (Mul (Cst 1) (Cst 2)) (Div (Cst 3) (Cst 4)))

subWithEqAdPriority :: Exp
subWithEqAdPriority = (Sub (Sub (Cst 1) (Cst 2)) (Add (Cst 3) (Cst 4)))

subWithEqSubPriority :: Exp
subWithEqSubPriority = (Sub (Add (Cst 1) (Cst 2)) (Sub (Cst 3) (Cst 4)))

subWithHiPriority :: Exp
subWithHiPriority = (Sub (Mul (Cst 1) (Cst 2)) (Div (Cst 3) (Cst 4)))

subWithLoHiPriority :: Exp
subWithLoHiPriority = (Sub (Add (Cst 1) (Cst 2)) (Div (Cst 3) (Cst 4)))

subWithHiLoPriority :: Exp
subWithHiLoPriority = (Sub (Div (Cst 1) (Cst 2)) (Sub (Cst 3) (Cst 4)))

mulWithLoPriority :: Exp
mulWithLoPriority = (Mul (Add (Cst 1) (Cst 2)) (Sub (Cst 3) (Cst 4)))

mulWithEqPriority :: Exp
mulWithEqPriority = (Mul (Mul (Cst 1) (Cst 2)) (Div (Cst 3) (Cst 4)))

mulWithHiPriority :: Exp
mulWithHiPriority = (Mul (Pow (Cst 1) (Cst 2)) (Pow (Cst 3) (Cst 4)))

mulWithMixPriority :: Exp
mulWithMixPriority = (Mul (Add (Cst 1) (Cst 2)) (Pow (Cst 3) (Cst 4)))

divWithLoPriority :: Exp
divWithLoPriority = (Div (Add (Cst 1) (Cst 2)) (Sub (Cst 3) (Cst 4)))

divWithEqPriority :: Exp
divWithEqPriority = (Div (Mul (Cst 1) (Cst 2)) (Div (Cst 3) (Cst 4)))

divWithHiPriority :: Exp
divWithHiPriority = (Div (Pow (Cst 1) (Cst 2)) (Pow (Cst 3) (Cst 4)))

divWithMixPriority :: Exp
divWithMixPriority = (Div (Add (Cst 1) (Cst 2)) (Pow (Cst 3) (Cst 4)))

powWithLoPriority :: Exp
powWithLoPriority = (Pow (Div (Cst 1) (Cst 2)) (Sub (Cst 3) (Cst 4)))

powWithEqPriority :: Exp
powWithEqPriority = (Pow (Pow (Cst 1) (Cst 2)) (Pow (Cst 3) (Cst 4)))

powWithMixPriority :: Exp
powWithMixPriority = (Pow (Add (Cst 1) (Cst 2)) (Pow (Cst 3) (Cst 4)))


testShowCompact = testGroup "Unit tests for showCompact"
  [
    testCase "Constant value" $
      showCompact (Cst 87) @?= "87",

    testCase "Addition with arguments with same priority" $
      showCompact addWithEqPriority @?= "1 + 2 + 3 - 4",

    testCase "Addition with arguments with higher priority" $
      showCompact addWithMixPriority @?= "1 + 2 + 3 div 4",
    
    testCase "Addition with arguments with higher priority" $
      showCompact addWithHiPriority @?= "1 * 2 + 3 div 4",
    
    testCase "Subtraction with arguments of equal priority and addition" $
      showCompact subWithEqAdPriority @?= "1 - 2 - (3 + 4)",
    
    testCase "Subtraction with arguments of equal priority and subtraction" $
      showCompact subWithEqSubPriority @?= "1 + 2 - (3 - 4)",
    
    testCase "Subtraction with arguments of higher priority" $
      showCompact subWithHiPriority @?= "1 * 2 - 3 div 4",
    
    testCase "Subtraction with arguments with lower and higher priority" $
      showCompact subWithLoHiPriority @?= "1 + 2 - 3 div 4",
    
    testCase "Subtraction with arguments with higher and lower priority" $
      showCompact subWithHiLoPriority @?= "1 div 2 - (3 - 4)",
    
    testCase "Multiplication with arguments with lower priority" $
      showCompact mulWithLoPriority @?= "(1 + 2) * (3 - 4)",

    testCase "Multiplication with arguments with same priority" $
      showCompact mulWithEqPriority @?= "1 * 2 * 3 div 4",

    testCase "Multiplication with arguments with higher priority" $
      showCompact mulWithHiPriority @?= "1 ^ 2 * 3 ^ 4",

    testCase "Multiplication with mixed priority" $
      showCompact mulWithMixPriority @?= "(1 + 2) * 3 ^ 4",
    
    testCase "Division with arguments with lower priority" $
      showCompact divWithLoPriority @?= "(1 + 2) div (3 - 4)",

    testCase "Division with arguments with same priority" $
      showCompact divWithEqPriority @?= "1 * 2 div 3 div 4",

    testCase "Division with arguments with higher priority" $
      showCompact divWithHiPriority @?= "1 ^ 2 div 3 ^ 4",

    testCase "Division with mixed priority" $
      showCompact divWithMixPriority @?= "(1 + 2) div 3 ^ 4",
    
    testCase "Exponentiation with arguments with lower priority" $
      showCompact powWithLoPriority @?= "(1 div 2) ^ (3 - 4)",

    testCase "Exponentiation with arguments with same priority" $
      showCompact powWithEqPriority @?= "1 ^ 2 ^ 3 ^ 4",

    testCase "Exponentiation with mixed priority" $
      showCompact powWithMixPriority @?= "(1 + 2) ^ 3 ^ 4"
  ]


-- ----------------------- --
--  Tests for Problem 4.3  --
-- ----------------------- --

-- test just to ensure eager evaluation
testEvalEager = testGroup "Unit tests for evalEager"
  [
    testCase "Evaluating a let expression with an error in the definition" $
      evalEager (Let "x" (Var "y") (Cst 0)) initEnv @?= Left (EBadVar "y")
  ]


-- test just to ensure lazy evaluation
testEvalLazy = testGroup "Unit tests for evalLazy"
  [
    testCase "Evaluating a let expression with an error in the definition" $
      evalLazy (Let "x" (Var "y") (Cst 0)) initEnv @?= Right 0
  ]


-- ----------------------------------------------------------------------------

tests = testGroup "Tests" [testShowExp, testEvalSimple, testEvalFull, 
                            testExtendEnv, testEvalErr, testShowCompact, 
                            testEvalEager, testEvalLazy]

main :: IO ()
main = defaultMain tests
