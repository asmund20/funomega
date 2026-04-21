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
    , "data"
    , "fun"
    ]

reservedOpNames :: [String]
reservedOpNames =
    [ "."
    , "="
    , ":"
    ]

parseCommand :: Prompt -> Either ParseError Command
parseCommand = parse (command <* eof) "command"

command :: Parser Command
command =
    (char ':' >> (load <|> reload <|> help <|> quit))
        <|> eval
  where
    load :: Parser Command
    load = strings "l" >> Load <$> many anyChar
    reload :: Parser Command
    reload = strings "r" >> return Reload
    eval :: Parser Command
    eval = Eval <$> term
    help :: Parser Command
    help = strings "?" >> return Help
    quit :: Parser Command
    quit = strings "q" >> return Quit

parseTerm :: String -> Either ParseError Term
parseTerm = parse (term <* eof) "term"

parseDefinitions :: FilePath -> Either ParseError [Definition]
parseDefinitions = undefined

definition :: Parser Definition
definition = dataDef <|> binOpDef <|> varDef
  where
    dataDef :: Parser Definition
    dataDef = do
        strings "data"
        dataName <- dataTypeName
        vars <- many variableName
        strings "="
        constructors <-
            many1 (strings "|" >> pair <$> constructorName <*> many typeParser)
        return $ DataDef dataName vars constructors
    binOpDef :: Parser Definition
    binOpDef = do
        fixity
        nat
        x0 <- variableName
        op <- binOpName
        x1 <- variableName
        strings "="
        t <- term
        -- TODO: Need to use other names here, preferably ones that are not
        -- allowed
        return $ VarDef op (TypeVar "a" :->: (TypeVar "b" :->: TypeVar "c")) t
    varDef :: Parser Definition
    varDef =
        VarDef
            <$> ( variableName
                    *> strings ":"
                )
            <*> typeParser
            <*> (strings "=" >> term)
    pair a b = (a, b)

typeParser :: Parser Type
typeParser = chainr1 typeLit typeArrow
  where
    typeLit = (Prim <$> dataTypeName <*> many typeVar) <|> typeVar
    typeVar = TypeVar <$> variableName
    typeArrow = strings "->" >> return (:->:)

-- Must use buildExpressionParser here
term :: Parser Term
term = undefined

strings :: String -> Parser String
strings s = string s <* spaces

fixity :: Parser Fixity
fixity = string "infix" >> (parseInfix <|> parseInfixL <|> parseInfixR) <* spaces
  where
    parseInfix = space >> return Infix
    parseInfixL = char 'l' >> space >> return InfixL
    parseInfixR = char 'r' >> space >> return InfixR

binOpName :: Parser Op
binOpName = many1 (satisfy binOpChar) <* spaces >>= check
  where
    binOpChar c =
        (not $ isControl c)
            && (not $ isAlphaNum c)
            && (not $ isSpace c)
            && (not $ c == '(')
            && (not $ c == ')')
    check name
        | name `elem` reservedOpNames =
            parserFail $ '\'' : name ++ "' cannot be used as an operator name"
        | otherwise = return name

variableName :: Parser X
variableName = (:) <$> lower <*> many alphaNum <* spaces >>= check
  where
    check name
        | name `elem` reservedVariableNames =
            parserFail $ '\'' : name ++ "' cannot be used as a variable name"
        | otherwise = return name

upperThenAlphaNum :: Parser String
upperThenAlphaNum = (:) <$> upper <*> many alphaNum <* spaces

constructorName :: Parser C
constructorName = upperThenAlphaNum

dataTypeName :: Parser D
dataTypeName = upperThenAlphaNum

nat :: Parser Int
nat = zero <|> nonzero
  where
    zero = char '0' >> return 0 <* spaces
    nonzero = read <$> many1 digit <* spaces

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
