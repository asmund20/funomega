module Main (main) where

import Test.Tasty

import InterpreterTests (testInterpreter)
import ParserTests
import TypeCheckerTests (testTypeChecker)

main :: IO ()
main =
    defaultMain $
        localOption (mkTimeout 30000000) $
            testGroup "Test Suite:" $
                [testParser, testTypeChecker, testInterpreter]
