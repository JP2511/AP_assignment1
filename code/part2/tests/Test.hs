-- Very rudimentary test of Arithmetic. Feel free to replace completely

import Definitions
import Arithmetic

import Data.List (intercalate)
import System.Exit (exitSuccess, exitFailure)  -- for when running stand-alone
import Test.Tasty
import Test.Tasty.HUnit

tests :: [(String, Bool)]
tests = [test1, test2, test3] where
  test1 = ("test1", evalSimple (Add (Cst 2) (Cst 2)) == 4)
  test2 = ("test2", evalFull (Let "a" (Cst 42) (Var "a")) initEnv == 42)
  test3 = ("test3", evalErr (Var "x") initEnv == Left (EBadVar "x"))


{- Complex Expression which we couldn't fit inside of a testCase because where
  doesn't work inside of a testCase
-}
divExp = Div (Cst 1) (Cst 2)
mulExp = Mul (Add (Cst 3) (Cst 4)) (Sub (Cst 6) (Cst 5))
complexExp = Pow divExp mulExp
complexShowResult = "((1 div 2) ^ ((3 + 4) * (6 - 5)))"


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


-- Environment with multiple 
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


main :: IO ()
main =
  let failed = [name | (name, ok) <- tests, not ok]
  in case failed of
       [] -> do putStrLn "All tests passed!"
                exitSuccess
       _ -> do putStrLn $ "Failed tests: " ++ intercalate ", " failed
               exitFailure
