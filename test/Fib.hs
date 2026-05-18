module Fib where

import Control.Monad (forM_)
import Data.List (intercalate)
import Interpreter
import Parser
import Syntax
import qualified Test.QuickCheck.Monadic as Monadic
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import TypeChecker

makeNumber :: Int -> Term
makeNumber 0 = Constructor "Z" []
makeNumber n = Constructor "S" [makeNumber $ n - 1]

checkTerms :: [Term] -> Program -> Either TypeError ()
checkTerms ts p = forM_ ts (`runTermCheck` p)

parenEsrap :: Term -> String
parenEsrap t = "(" ++ esrap t ++ ")"

esrap :: Term -> String
esrap (Variable x) = x
esrap (Constructor c ts) = let ts' = intercalate " " $ map parenEsrap ts in c ++ " " ++ ts'
esrap (Application t0 t1) = esrap t0 ++ " " ++ esrap t1
esrap (Case t cs) = undefined
esrap (Function x t) = undefined

testPlus :: Int -> Int -> Property
testPlus i1 i2 = Monadic.monadicIO $ do
    fibSource <- Monadic.run (readFile "examples/fib.fomega")
    let n1 = makeNumber i1
        n2 = makeNumber i2
        result = makeNumber $ i1 + i2
        def_and_table = parseDefinitions fibSource

    case def_and_table of
        Left e -> do
            Monadic.monitor $ counterexample $ show e
            Monadic.assert False
        Right (defs, table) -> do
            case checkProgram defs of
                Left e -> do
                    Monadic.monitor $ counterexample $ show e
                    Monadic.assert False
                Right prog -> do
                    let commandString = esrap n1 ++ " + " ++ esrap n2
                        command = parseCommand table commandString
                    case command of
                        Left e -> do
                            Monadic.monitor $ counterexample $ show e ++ "\nCommand is: " ++ commandString
                            Monadic.assert False
                        Right (Eval term) -> case checkTerms [n1, n2, term] prog of
                            Left e -> do
                                Monadic.monitor $ counterexample $ show e
                                Monadic.assert False
                            Right () -> do
                                case interpretTerm prog term of
                                    Left e -> do
                                        Monadic.monitor $ counterexample $ show e
                                        Monadic.assert False
                                    Right v -> do
                                        -- TODO
                                        Monadic.assert $ toTerm v == result
                        Right c -> do
                            Monadic.monitor $
                                counterexample $
                                    "Parsed a term, got other command: " ++ show c

fib_tests :: TestTree
fib_tests =
    testGroup
        "Fibonnaci test"
        [ localOption (QuickCheckMaxSize 20) $
            localOption (QuickCheckTests 2) $
                testProperty "plus" testPlus
        ]
