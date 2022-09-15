-- Very rudimentary test of Arithmetic. Feel free to replace completely

import Definitions
import Arithmetic

import Data.List (intercalate)
import System.Exit (exitSuccess, exitFailure)  -- for when running stand-alone
import Test.Tasty
import Test.Tasty.HUnit
import Definitions (ArithError(EOther))


tests :: [(String, Bool)]
tests = [test1, test2, test3] where
  test1 = ("test1", evalSimple (Add (Cst 2) (Cst 2)) == 4)
  test2 = ("test2", evalFull (Let "a" (Cst 42) (Var "a")) initEnv == 42)
  test3 = ("test3", evalErr (Var "x") initEnv == Left (EBadVar "x"))

testEvalErr = testGroup "Unit tests for evalErr"
  [ testCase "Testing evalErr: Constant expression" $
      evalErr (Cst 5) initEnv @?= Right 5 ,
    
    testCase "Testing evalErr: Division by 0" $
      evalErr (Div (Cst 5) (Cst 0)) initEnv @?= Left EDivZero,
    
    testCase "Testing evalErr: Division by a non-zero value" $
      evalErr (Div (Cst 5) (Cst 2)) initEnv @?= Right 2,
    
    testCase "Testing evalErr: Division where denominater is larger than nominater" $
      evalErr (Div (Cst 5) (Cst 6)) initEnv @?= Right 0,
    
    testCase "Testing evalErr: Multiplication" $
      evalErr (Mul (Cst 5) (Cst 4)) initEnv @?= Right 20,
    
    testCase "Testing evalErr: Subtraction" $
      evalErr (Sub (Cst 79) (Cst 80)) initEnv @?= Right (-1),
    
    testCase "Testing evalErr: Subtraction" $
      evalErr (Add (Cst 79) (Cst 21)) initEnv @?= Right 100,

    testCase "Testing evalErr: Exponential" $
      evalErr (Pow (Cst 3) (Cst 3)) initEnv @?= Right 27,

    testCase "Testing evalErr: Negative Exponential" $
      evalErr (Pow (Cst 79) (Cst (-2))) initEnv @?= Left ENegPower,
    
    testCase "Testing evalErr: If statment, condition == 0" $
      evalErr (If (Cst 0) (Cst 3) (Cst 2)) initEnv @?= Right 2,

    testCase "Testing evalErr: If statment, condition /= 0" $
      evalErr (If (Cst 100) (Cst 3) (Cst 2)) initEnv @?= Right 3,
    
    testCase "Testing evalErr: If statment, condition /= 0" $
      evalErr (If (Cst 100) (Cst 3) (Cst 2)) initEnv @?= Right 3,

    testCase "Testing evalErr: Sum" $
      evalErr (Sum "x" (Cst 1) (Cst 4) (Mul (Cst 10) (Var "x"))) initEnv @?= Right 100,

    testCase "Testing evalErr: Sum where 'Start value' larger than 'End value'" $
      evalErr (Sum "x" (Cst 6) (Cst 4) (Mul (Cst 10) (Var "x"))) initEnv @?= Left (EOther "Sum -> Initial value bigger than final value"),
    
    
    testCase "Testing evalErr: Let where the variable is divided by 0" $
      evalErr (Let "x" (Div (Cst 4) (Cst 0)) (Cst 5)) initEnv @?= Left EDivZero,

    testCase "Testing evalErr: Let" $
      evalErr (Let "x" (Cst 4) (Add (Cst 5) (Var "x"))) initEnv @?= Right 9,

    testCase "Testing evalErr: Var not in enviroment" $
      evalErr (Var "x") initEnv @?= Left (EBadVar "x"),

    testCase "Testing evalErr: Var in the enviroment" $
      evalErr (Var "x") (extendEnv "x" 4 initEnv) @?= Right 4 ]


main :: IO ()
main =
  let failed = [name | (name, ok) <- tests, not ok]
  in case failed of
       [] -> do putStrLn "All tests passed!"
                exitSuccess
       _ -> do putStrLn $ "Failed tests: " ++ intercalate ", " failed
               exitFailure
