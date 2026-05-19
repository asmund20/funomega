module ParserTests where

import Parser
import Syntax
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

parsedEqually :: OpTable -> String -> String -> Assertion
parsedEqually table t1 t2 =
    case parseCommand table t1 of
        Left e -> assertFailure $ show e
        Right (Eval term1) -> case parseCommand table t2 of
            Left e -> assertFailure $ show e
            Right (Eval term2) -> assertEqual "terms parsed differently" term1 term2
            Right c -> assertFailure $ "Expected to parse as term, got: " ++ show c
        Right c -> assertFailure $ "Expected to parse as term, got: " ++ show c

parsedDifferent :: OpTable -> String -> String -> Assertion
parsedDifferent table t1 t2 =
    case parseCommand table t1 of
        Left e -> assertFailure $ show e
        Right term1 -> case parseCommand table t2 of
            Left e -> assertFailure $ show e
            Right term2 -> assertDifferent term1 term2

assertDifferent :: (Eq a, Show a) => a -> a -> Assertion
assertDifferent l r =
    if l /= r
        then return ()
        else assertFailure $ "Should be different: " ++ show l ++ "\n" ++ show r

parseTo :: OpTable -> String -> Command -> Assertion
parseTo table t c = case parseCommand table t of
    Left e -> assertFailure $ show e
    Right c' -> assertEqual "Parsed to wrong thing" c c'

parseFail :: OpTable -> String -> Assertion
parseFail table t = case parseCommand table t of
    Left _ -> pure ()
    Right r -> assertFailure $ "Should fail to parse " ++ t ++ "\nParsed to: " ++ show r

testAssociativity :: Assertion
testAssociativity = do
    source <- readFile "examples/numbers.fomega"
    case parseDefinitions source of
        Left e -> assertFailure $ show e
        Right (_, table) -> do
            parsedEqually
                table
                "S Z + Z - S (S Z) + (S (S Z))"
                "((S Z + Z) - S (S Z)) + (S (S Z))"
            parseFail table "S Z == S (S Z) == n5"

testPrecedence :: Assertion
testPrecedence = do
    source <- readFile "examples/numbers.fomega"
    case parseDefinitions source of
        Left e -> assertFailure $ show e
        Right (_, table) -> do
            parsedEqually
                table
                "n5 + n1 * n4 - S Z"
                "n5 + (n1 *n4) - (S Z)"
            parseFail table "S Z == S (S Z) == n5"
            parsedDifferent table "n2 + n3 * n4" "(n2 + n3) * n4"

testBinaryOperator :: Assertion
testBinaryOperator = do
    source <- readFile "examples/numbers.fomega"
    case parseDefinitions source of
        Left e -> assertFailure $ show e
        Right (_, table) -> do
            parseTo table "n1 == n1" $
                Eval $
                    Application
                        (Application (Variable "==") (Variable "n1"))
                        (Variable "n1")

testMultipleTypeVariables :: Assertion
testMultipleTypeVariables = do
    case parseDefinitions "data Test a b = | Dummy a b ." of
        Left e -> assertFailure $ show e
        Right (ds, _) ->
            assertEqual
                "Fail"
                ds
                [DataDef "Test" ["a", "b"] [("Dummy", [TypeVar "a", TypeVar "b"])]]

testSamePrecDiffAssoc :: Assertion
testSamePrecDiffAssoc = do
    source <- readFile "test/failureCases/precAssocConflict.fomega"
    case parseDefinitions source of
        Left _ -> return ()
        Right _ -> assertFailure "Fail"

testEmpty :: Assertion
testEmpty = do
    source <- readFile "examples/empty.fomega"
    case parseDefinitions source of
        Left e -> assertFailure $ show e
        Right _ -> return ()

testParser :: TestTree
testParser =
    testGroup
        "Parser test for funOmega"
        [ testCase "Assiciativity" testAssociativity
        , testCase "Precedence" testPrecedence
        , testCase "Binary operator" testBinaryOperator
        , testCase "Same precedence different associativity" testSamePrecDiffAssoc
        , testCase "Empty program" testEmpty
        , testCase "Multiple type variables" testMultipleTypeVariables
        ]
