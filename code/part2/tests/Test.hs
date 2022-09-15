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

{- Complex Expression which I couldn't fit inside of a testCase because where
  doesn't work inside of a testCase
-}
divExp = Div (Cst 1) (Cst 2)
mulExp = Mul (Add (Cst 3) (Cst 4)) (Sub (Cst 6) (Cst 5))
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
      showExp (Pow divExp mulExp) @?= complexShowResult 
  ]

main :: IO ()
main =
  let failed = [name | (name, ok) <- tests, not ok]
  in case failed of
       [] -> do putStrLn "All tests passed!"
                exitSuccess
       _ -> do putStrLn $ "Failed tests: " ++ intercalate ", " failed
               exitFailure
