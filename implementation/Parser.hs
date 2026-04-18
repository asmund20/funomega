-- Fill in the `undefined` part, feel free to make other changes.
-- Justify your changes in the report.

module Parser where

import Syntax
import Text.Parsec

parseCommand :: Prompt -> Either ParseError Command
parseCommand = undefined

parseTerm :: String -> Either ParseError Term
parseTerm = undefined

parseDefinitions :: FilePath -> Either ParseError [Definition]
parseDefinitions = undefined
