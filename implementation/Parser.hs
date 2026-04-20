-- Fill in the `undefined` part, feel free to make other changes.
-- Justify your changes in the report.

module Parser (parseDefinitions, parseInfixes, parseCommand, parseTerm) where

import Data.Char (isAlphaNum, isControl, isSpace)
import Data.List (groupBy, sortBy)
import Data.Maybe (catMaybes)
import Syntax
import Text.Parsec (parserFail)
import Text.ParserCombinators.Parsec

reservedVariableNames :: [String]
reservedVariableNames =
    [ "case"
    , "of"
    , "infix"
    , "infixl"
    , "infixr"
    ]

reservedOpNames :: [String]
reservedOpNames =
    [ "."
    , "="
    , ":"
    ]

parseCommand :: Prompt -> Either ParseError Command
parseCommand = undefined

parseTerm :: String -> Either ParseError Term
parseTerm = undefined

parseDefinitions :: FilePath -> Either ParseError [Definition]
parseDefinitions = undefined

strings :: String -> Parser String
strings s = string s <* spaces

fixity :: Parser Fixity
fixity = string "infix" >> (parseInfix <|> parseInfixL <|> parseInfixR) <* spaces
  where
    parseInfix = space >> return Infix
    parseInfixL = char 'l' >> space >> return InfixL
    parseInfixR = char 'r' >> space >> return InfixR

-- TODO: Make sure that it is not a reserved name (reservedNames).
binOpName :: Parser Op
binOpName = many1 (satisfy binOpChar) <* spaces
  where
    binOpChar c =
        (not $ isControl c)
            && (not $ isAlphaNum c)
            && (not $ isSpace c)
            && (not $ c == '(')
            && (not $ c == ')')

variableName :: Parser X
variableName = (:) <$> lower <*> many alphaNum >>= check
  where
    check name
        | name `elem` reservedVariableNames =
            parserFail $ '\'' : name ++ "cannot be used as a variable name"
        | otherwise = return name

-- do
--     c <- lower
--     cs <- many alphaNum
--     let name = c : cs
--     if name `elem` reservedNames then undefined else return name

constructorName :: Parser C
constructorName = undefined

dataTypeName :: Parser D
dataTypeName = undefined

-- Under here is the code for the first pass of the parsing, which is extracting
-- the infix operators.

-- | Parse the infix operators
parseInfixes :: String -> Either ParseError OpTable
parseInfixes s = do
    operators <- parse (infixity <* eof) "infixes" s
    let groups = groupBy grouping operators
        sortedGroups = map (sortBy sorting) groups
        withoutPrec = map (map removePrec) sortedGroups
        extractedInfixity = map extractInfixity withoutPrec

    return $ OpTable $ map extractInfixity withoutPrec
  where
    grouping (a, _, _) (b, _, _) = a == b
    sorting (_, l, _) (_, r, _) = compare l r
    removePrec (f, _, n) = (f, n)
    extractInfixity l@((i, _) : _) = (i, map second l)
    second (_, v) = v

    infixity :: Parser [(Fixity, Prec, Op)]
    infixity = catMaybes <$> many1 (binOpDef <|> otherDef)

    binOpDef :: Parser (Maybe (Fixity, Prec, Op))
    binOpDef =
        Just
            <$> ( fixityEx
                    <*> (nat <* variableName)
                    <*> binOpName
                    <* manyTill anyChar (strings ".")
                )

    otherDef :: Parser (Maybe (Fixity, Prec, Op))
    otherDef = manyTill anyChar (strings ".") >> return Nothing

    fixityEx :: Parser (Prec -> Op -> (Fixity, Prec, Op))
    fixityEx = triple <$> fixity
      where
        triple fst snd trd = (fst, snd, trd)

    nat :: Parser Int
    nat = zero <|> nonzero
      where
        zero = char '0' >> return 0 <* spaces
        nonzero = read <$> many1 digit <* spaces
