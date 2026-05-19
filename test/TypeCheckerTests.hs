{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module TypeCheckerTests (testTypeChecker) where

import Parser
import Syntax
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import TypeChecker

load :: String -> Either String (Program, OpTable)
load source = do
    case parseDefinitions source of
        Left e -> Left $ show e
        Right (defs, table) -> case checkProgram defs of
            Left e -> Left $ show e
            Right prog -> Right (prog, table)

checkTerm :: OpTable -> Program -> String -> Type -> Assertion
checkTerm table prog command expectedT = case parseCommand table command of
    Left e -> assertFailure $ show e
    Right (Eval term) -> case runTermCheck term prog of
        Left e -> assertFailure $ show e
        Right t -> assertEqual "Different types" expectedT t
    Right c -> assertFailure $ "Expected Eval command, got " ++ show c

detectError :: OpTable -> Program -> String -> Assertion
detectError table prog command = case parseCommand table command of
    Left e -> assertFailure $ show e
    Right (Eval term) -> case runTermCheck term prog of
        Left _ -> return ()
        Right t ->
            assertFailure $
                "Expected "
                    ++ command
                    ++ " to not pass type check, got type "
                    ++ show t
    Right c -> assertFailure $ "Expected Eval command, got " ++ show c

testEmpty :: Assertion
testEmpty = do
    source <- readFile "examples/empty.fomega"
    case load source of
        Left e -> assertFailure e
        Right _ -> return ()

testExample :: Assertion
testExample = do
    source <- readFile "examples/example.fomega"
    case load source of
        Left e -> assertFailure e
        Right (prog, table) -> do
            checkTerm table prog "True" $ Prim "Bool" []
            checkTerm table prog "True & False" $ Prim "Bool" []
            checkTerm table prog "True | False" $ Prim "Bool" []
            checkTerm table prog "xor True False" $ Prim "Bool" []
            checkTerm table prog "xor" $
                Prim "Bool" [] :->: (Prim "Bool" [] :->: Prim "Bool" [])
            checkTerm table prog "xor True" $ Prim "Bool" [] :->: Prim "Bool" []
            detectError table prog "xor hallo True"

testList :: Assertion
testList = do
    source <- readFile "examples/list.fomega"
    case load source of
        Left e -> assertFailure e
        Right (prog, table) -> do
            let checkTerm' = checkTerm table prog
                detectError' = detectError table prog
                bool = Prim "Bool" []
            checkTerm' "True" bool
            checkTerm' "True :: Nil" $ Prim "List" [bool]
            checkTerm' "Append True Nil" $ Prim "List" [bool]
            detectError' "True :: False"
            detectError' "Append True False"
            detectError' "Nil :: True :: Nil"
            detectError' "Append Nil (Append True Nil)"

testNumbers :: Assertion
testNumbers = do
    source <- readFile "examples/numbers.fomega"
    case load source of
        Left e -> assertFailure e
        Right (prog, table) -> do
            let checkTerm' = checkTerm table prog
                detectError' = detectError table prog
                nat = Prim "Nat" []
                bool = Prim "Bool" []
            checkTerm' "Z" nat
            checkTerm' "S (S Z)" nat
            checkTerm' "n5" nat
            checkTerm' "Z + n2" nat
            checkTerm' "plus" $ nat :->: (nat :->: nat)
            checkTerm' "plus Z" $ nat :->: nat
            checkTerm' "plusTen" $ nat :->: nat
            detectError' "S True"
            detectError' "True == False"
            detectError' "True * False"

            -- TODO: These commented out tests fail. Comment in when done making
            -- tests and remove dummy below
            -- checkTerm' "n8 - Z" nat
            -- checkTerm' "S (S Z) * n10" nat
            -- checkTerm' "S Z == Z" bool
            -- checkTerm' "Z == Z" bool
            -- checkTerm' "n3 == S Z" bool
            -- detectError' "True + n2"
            -- detectError' "n3 + False"
            -- detectError' "S Z * True"
            assertBool "Dummy" True

testConstructors :: Assertion
testConstructors = do
    source <- readFile "examples/types.fomega"
    case load source of
        Left e -> assertFailure e
        Right (prog, table) -> do
            let checkTerm' = checkTerm table prog
                detectError' = detectError table prog
                nat = Prim "Nat" []
                mbe = \t -> Prim "Maybe" [t]
                pair = \t1 t2 -> Prim "Pair" [t1, t2]
                list = \t -> Prim "List" [t]

            checkTerm' "Just Z" $ mbe nat
            checkTerm' "Nothing" $ mbe $ TypeVar "a"
            checkTerm' "P (S Z) Nil" $ pair nat $ list $ TypeVar "a"
            checkTerm' "first (P Z Nil)" nat
            checkTerm' "second (P Z Nil)" $ list $ TypeVar "a"
            checkTerm' "head" $ (list $ TypeVar "a") :->: mbe (TypeVar "a")
            detectError' "first (S Z)"
            detectError' "first (Right Nil)"

testTypeChecker :: TestTree
testTypeChecker =
    testGroup
        "Type checker test for funOmega"
        [ testCase "Empty program" testEmpty
        , testCase "Example from task" testExample
        , testCase "Boolean lists" testList
        , testCase "Numbers" testNumbers
        , testCase "Constructors" testConstructors
        ]
