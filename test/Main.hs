module Main (main) where

import Test.Tasty

import ParserTests
import TypeCheckerTests (testTypeChecker)

main :: IO ()
main =
    defaultMain $
        localOption (mkTimeout 30000000) $
            testGroup "Test Suite:" $
                [testParser, testTypeChecker]
