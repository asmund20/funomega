-- Justify your changes in the report.

module Parser (parseDefinitions, parseInfixes, parseCommand, parseTerm) where

import Data.Char (isAlphaNum, isControl, isSpace)
import Data.Functor.Identity
import Data.List (groupBy, sortBy)
import Data.Maybe (catMaybes)
import Syntax
import Text.Parsec (parserFail)
import Text.Parsec.Error (Message (Message), newErrorMessage)
import Text.Parsec.Pos (initialPos)
import Text.Parsec.Prim (ParsecT)
import Text.Parsec.Token (GenTokenParser (reservedOp))
import Text.ParserCombinators.Parsec

-- Reserved names

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

-- Exported parsing functions

parseCommand :: OpTable -> Prompt -> Either ParseError Command
parseCommand table = parse (command (makeTermParser table) <* eof) "command"

command :: Parser Term -> Parser Command
command term =
    (char ':' >> (load <|> reload <|> help <|> quit))
        <|> eval term
  where
    load :: Parser Command
    load = strings "l" >> Load <$> many anyChar
    reload :: Parser Command
    reload = strings "r" >> return Reload
    eval :: Parser Term -> Parser Command
    -- TODO: Should be some value somewhere maybe
    eval term = Eval <$> term
    help :: Parser Command
    help = strings "?" >> return Help
    quit :: Parser Command
    quit = strings "q" >> return Quit

-- TODO: Probably delete this
parseTerm :: OpTable -> String -> Either ParseError Term
parseTerm table = parse (makeTermParser table <* eof) "term"

-- TODO: Might want to do the IO in the driver
parseDefinitions ::
    FilePath -> IO (Either ParseError ([Definition], OpTable))
parseDefinitions f = do
    source <- readFile f
    case parseInfixes source of
        Right table -> do
            let term = makeTermParser table
            case parse (definitions term <* eof) f source of
                Right defs -> return $ Right (defs, table)
                Left e -> return $ Left e
        Left p -> return $ Left p
  where
    definitions term = sepBy (definition term) (strings ".")

definition :: Parser Term -> Parser Definition
definition term = dataDef <|> binOpDef term <|> varDef term
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
    binOpDef :: Parser Term -> Parser Definition
    binOpDef term = do
        fixity
        nat
        x0 <- variableName
        op <- binOpName
        x1 <- variableName
        strings "="
        t <- term
        -- TODO: Depending on type inference algorithm, I might need to use
        -- other names here, preferably ones that are not allowed
        return $
            VarDef
                op
                (TypeVar "a" :->: (TypeVar "b" :->: TypeVar "c"))
                (Function x0 (Function x1 t))
    varDef :: Parser Term -> Parser Definition
    varDef term =
        VarDef
            <$> ( variableName
                    *> strings ":"
                )
            <*> typeParser
            <*> (strings "=" >> term)
    pair a b = (a, b)

-- The parsers used in second pass

value :: Parser Term -> Parser Value
value term = constructorValue term <|> lambdaValue term
  where
    constructorValue term = Value <$> constructorName <*> many (value term)
    lambdaValue term =
        strings "\\"
            >> Lambda <$> variableName <*> (strings "." >> term)

makeTermParser :: OpTable -> Parser Term
makeTermParser table = termParser table table
  where
    -- Partial table -> Full table -> Parser Term
    termParser :: OpTable -> OpTable -> Parser Term
    termParser (OpTable []) fullTable = appOrCons fullTable
    termParser (OpTable ((InfixL, ops) : rest)) fullTable =
        chainl1 (termParser (OpTable rest) fullTable) (opParser ops)
    termParser (OpTable ((InfixR, ops) : rest)) fullTable =
        chainr1 (termParser (OpTable rest) fullTable) (opParser ops)
    termParser (OpTable ((Infix, ops) : rest)) fullTable = do
        let subTerm = termParser (OpTable rest) fullTable
        t1 <- subTerm
        o <- opParser ops
        (o t1) <$> subTerm
    opParser :: [Op] -> Parser (Term -> Term -> Term)
    opParser ops = do
        o <- choice $ map (try . strings) ops
        return (\t1 t2 -> Application (Application (Variable o) t1) t2)

appOrCons :: OpTable -> Parser Term
appOrCons table =
    ( Constructor
        <$> constructorName
        <*> many (literal table)
    )
        <|> (chainl1 (literal table) (return Application))
  where
    literal table =
        choice
            [ functionTerm table
            , caseTerm table
            , parenTerm table
            , Variable <$> variableName
            , Constructor <$> constructorName <*> pure []
            ]
    functionTerm :: OpTable -> Parser Term
    functionTerm table =
        try $
            strings "fun"
                >> Function <$> variableName <*> (strings "->" >> makeTermParser table)
    caseTerm :: OpTable -> Parser Term
    caseTerm table =
        try $
            strings "case"
                >> Case
                    <$> makeTermParser table
                    <*> ( strings "of"
                            >> many (caseInstance table)
                        )
    parenTerm :: OpTable -> Parser Term
    parenTerm table = strings "(" *> (makeTermParser table) <* strings ")"
    caseInstance :: OpTable -> Parser (Pattern, Term)
    caseInstance table = strings ";" >> pair <$> pattern <*> (strings "->" >> (makeTermParser table))
    pair l r = (l, r)

pattern :: Parser Pattern
pattern = VarPat <$> variableName <|> ConPat <$> constructorName <*> many pattern

typeParser :: Parser Type
typeParser = chainr1 typeLit typeArrow
  where
    typeLit = (Prim <$> dataTypeName <*> many typeVar) <|> typeVar
    typeVar = TypeVar <$> variableName
    typeArrow = strings "->" >> return (:->:)

strings :: String -> Parser String
strings s = string s <* spaces

fixity :: Parser Fixity
fixity = string "infix" >> (parseInfix <|> parseInfixL <|> parseInfixR) <* spaces
  where
    parseInfix = space >> return Syntax.Infix
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

-- Below here are are the parsers used only for first pass

-- | Parse the infix operators
parseInfixes :: String -> Either ParseError OpTable
parseInfixes s = do
    operators <- parse (infixity <* eof) "infixes" s
    let sorted = reverse $ sortBy sorting operators
        groups = groupBy grouping sorted
        -- TODO: Test this with a function that just loops through the list
        -- instead of a second group by. Use grouping only by precedence, and
        -- check that all the fixities are the same.
        testGroups = groupBy testGrouping sorted
        withoutPrec = map (map removePrec) groups
        extractedInfixity = map extractInfixity withoutPrec

    if (testGroups == groups)
        then
            Right $ OpTable $ map extractInfixity withoutPrec
        else
            Left $
                newErrorMessage
                    ( Message
                        "Detected operators with same precedence but different associativity, not allowed"
                    )
                    (initialPos "infixes")
  where
    grouping :: (Fixity, Prec, Op) -> (Fixity, Prec, Op) -> Bool
    grouping (f1, p1, _) (f2, p2, _) = f1 == f2 && p1 == p2
    testGrouping :: (Fixity, Prec, Op) -> (Fixity, Prec, Op) -> Bool
    testGrouping (_, p1, _) (_, p2, _) = p1 == p2
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
    triple fst snd trd = (fst, snd, trd)
