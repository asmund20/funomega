module InterpreterTests (testInterpreter) where

import Interpreter
import Parser
import Syntax
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import TypeCheckerTests (load)

makeNumber :: Int -> String
makeNumber 0 = "Z"
makeNumber n = "(S " ++ makeNumber (n - 1) ++ ")"

testMakeNumber :: Assertion
testMakeNumber = do
    assertEqual "Zero" "Z" $ makeNumber 0
    assertEqual "Five" "(S (S (S (S (S Z)))))" $ makeNumber 5

execute :: Program -> OpTable -> String -> Either String Value
execute prog table command = do
    case parseCommand table command of
        Right (Eval term) -> interpretTerm prog term
        c -> Left $ "Expected Eval command, got " ++ show c

checkNumber :: OpTable -> Program -> String -> Int -> Assertion
checkNumber table prog command n = case execute prog table (makeNumber n) of
    Left e -> assertFailure e
    Right expectedV -> checkTerm table prog command expectedV

checkTerm :: OpTable -> Program -> String -> Value -> Assertion
checkTerm table prog command expectedV = case parseCommand table command of
    Left e -> assertFailure $ show e
    Right (Eval term) -> case interpretTerm prog term of
        Left e -> assertFailure $ command ++ "\n" ++ show e
        Right v -> assertEqual (command ++ "\n") expectedV v
    Right c -> assertFailure $ "Expected Eval command, got " ++ show c

checkTermFails :: OpTable -> Program -> String -> Assertion
checkTermFails table prog command = case parseCommand table command of
    Left e -> assertFailure $ show e
    Right (Eval term) -> case interpretTerm prog term of
        Left _ -> return ()
        Right v -> assertFailure $ command ++ "\n" ++ show v
    Right c -> assertFailure $ "Expected Eval command, got " ++ show c

testExample :: Assertion
testExample = do
    source <- readFile "examples/example.fomega"
    case load source of
        Left e -> assertFailure e
        Right (prog, table) -> do
            let checkTerm' = checkTerm table prog
            checkTerm' "True" $ Value "True" []
            checkTerm' "True & False" $ Value "False" []
            checkTerm' "True | False" $ Value "True" []
            checkTerm' "xor True False" $ Value "True" []
            checkTerm' "xor False True" $ Value "True" []
            checkTerm' "xor True True" $ Value "False" []
            checkTerm' "xor False False" $ Value "False" []

testNumbers :: Assertion
testNumbers = do
    source <- readFile "examples/numbers.fomega"
    case load source of
        Left e -> assertFailure e
        Right (prog, table) -> do
            let checkNumber' = checkNumber table prog
                checkTerm' = checkTerm table prog
            checkNumber' "Z" 0
            checkNumber' "S (S Z)" 2
            checkNumber' "n5" 5
            checkNumber' "Z + n2" 2
            checkNumber' "plus n15 n12" $ 15 + 12
            checkNumber' "plusTen n11" $ 11 + 10
            checkNumber' "n8 - Z" 8
            checkNumber' "n5 - n7" 0
            checkNumber' "n10 - n6" $ 10 - 6
            checkNumber' "Z * n12" $ 0
            checkNumber' "n14 ^ Z" $ 1
            checkNumber' "n6 ^ n0" $ 1
            checkTerm' "S Z == Z" $ Value "False" []
            checkTerm' "Z == Z" $ Value "True" []
            checkTerm' "n3 == S Z" $ Value "False" []
            checkTerm' "S (S (S (S Z))) == n4" $ Value "True" []
            checkTerm' ("n15 == " ++ makeNumber 15) $ Value "True" []
            checkNumber' "n10 * Z" $ 0
            checkNumber' "S (S Z) * n10" $ 2 * 10
            checkNumber' "n6 ^ n1" $ 6
            checkNumber' "n5 ^ n2" $ 5 ^ (2 :: Integer)

testNonExhaustive :: Assertion
testNonExhaustive = do
    source <- readFile "examples/nonExhaustive.fomega"
    case load source of
        Left e -> assertFailure e
        Right (prog, table) -> do
            let checkTerm' = checkTerm table prog
            checkTerm' "isZero Z" $ Value "Z" []
            checkTermFails table prog "isZero (S Z)"

testAlphaConversion :: Assertion
testAlphaConversion = do
    source <- readFile "examples/alpha.fomega"
    case load source of
        Left e -> assertFailure e
        Right (prog, table) -> do
            checkTerm table prog "t" $ Value "C" []

testInterpreter :: IO TestTree
testInterpreter =
    return $
        testGroup
            "Interpreter test for funOmega"
            [ testCase "Make number sanity" $ testMakeNumber
            , testCase "Example from task" testExample
            , testCase "Numbers" testNumbers
            , testCase "Non-exhaustive patterns" testNonExhaustive
            , testCase "Alpha conversion" testAlphaConversion
            ]
