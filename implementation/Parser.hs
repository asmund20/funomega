module Parser (parseDefinitions, parseInfixes, parseCommand) where

import Data.Char (isAlphaNum, isControl, isSpace)
import Data.Foldable (Foldable (toList))
import Data.List (sortBy)
import Data.List.NonEmpty (NonEmpty ((:|)), groupBy)
import Data.Maybe (catMaybes)
import Syntax
import Text.Parsec (parserFail)
import Text.Parsec.Error (Message (Message), newErrorMessage)
import Text.Parsec.Pos (initialPos)
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

parseDefinitions ::
    String -> Either ParseError ([Definition], OpTable)
parseDefinitions source = do
    case parseInfixes source of
        Right table -> do
            let term = makeTermParser table
            case parse (definitions term <* eof) "files" source of
                Right defs -> Right (defs, table)
                Left e -> Left e
        Left p -> Left p
  where
    definitions term = endBy (definition term) (strings ".")

definition :: Parser Term -> Parser Definition
definition term = dataDef <|> binOpDef <|> varDef
  where
    dataDef :: Parser Definition
    dataDef = do
        _ <- try $ strings "data"
        dataName <- dataTypeName
        vars <- many variableName
        _ <- strings "="
        constructors <-
            many1 (strings "|" >> pair <$> constructorName <*> many typeParser)
        return $ DataDef dataName vars constructors
    binOpDef :: Parser Definition
    binOpDef = do
        _ <- fixity
        _ <- nat
        x0 <- variableName
        op <- binOpName
        x1 <- variableName
        _ <- strings "="
        t <- term
        return $
            VarDef
                op
                (TypeVar op)
                (Function x0 (Function x1 t))
    varDef :: Parser Definition
    varDef =
        VarDef
            <$> ( variableName
                    <* strings ":"
                )
            <*> typeParser
            <*> (strings "=" >> term)
    pair a b = (a, b)

-- The parsers used in second pass

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
        m <- optionMaybe $ opParser ops
        case m of
            Nothing -> return t1
            Just o -> (o t1) <$> subTerm
    opParser :: [Op] -> Parser (Term -> Term -> Term)
    opParser ops = do
        o <- choice $ map (try . strings) ops
        return (\t1 t2 -> Application (Application (Variable o) t1) t2)

appOrCons :: OpTable -> Parser Term
appOrCons table =
    ( Constructor
        <$> constructorName
        <*> many literal
    )
        <|> (chainl1 literal (return Application))
  where
    literal =
        choice
            [ functionTerm
            , caseTerm
            , parenTerm
            , Variable <$> variableName
            , Constructor <$> constructorName <*> pure []
            ]
    functionTerm =
        try
            (string "fun" >> space >> spaces)
            >> Function <$> variableName <*> (strings "->" >> makeTermParser table)
    caseTerm =
        try
            (string "case" >> space >> spaces)
            >> Case
                <$> makeTermParser table
                <*> ( strings "of"
                        >> many caseInstance
                    )
    parenTerm = strings "(" *> (makeTermParser table) <* strings ")"
    caseInstance = strings ";" >> pair <$> pattern <*> (strings "->" >> (makeTermParser table))
    pair l r = (l, r)

pattern :: Parser Pattern
pattern = topLevel
  where
    topLevel =
        varPattern
            <|> topLevelConstructorPattern
            <|> parenPattern
    varPattern = VarPat <$> variableName
    topLevelConstructorPattern :: Parser Pattern
    topLevelConstructorPattern = ConPat <$> constructorName <*> many pattern
    parenPattern :: Parser Pattern
    parenPattern = strings "(" *> topLevel <* strings ")"

typeParser :: Parser Type
typeParser = chainr1 typeLit typeArrow
  where
    typeLit =
        (Prim <$> dataTypeName <*> many typeVar)
            <|> typeVar
            <|> (strings "(" *> typeParser <* strings ")")
    typeVar = TypeVar <$> variableName
    typeArrow = strings "->" >> return (:->:)

strings :: String -> Parser String
strings s = string s <* spaces

fixity :: Parser Fixity
fixity =
    try $ string "infix" >> (parseInfix <|> parseInfixL <|> parseInfixR) <* spaces
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
variableName = try $ (:) <$> lower <*> many alphaNum <* spaces >>= check
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
        withoutPrec = map (fmap removePrec) groups

    if (testGroups == groups)
        then
            OpTable <$> mapM extractInfixity withoutPrec
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
    extractInfixity l@((i, _) :| _) = Right (i, toList $ fmap second l)
    second (_, v) = v

    infixity :: Parser [(Fixity, Prec, Op)]
    infixity = catMaybes <$> many (binOpDef <|> otherDef)

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
    fixityEx = (\l m r -> (l, m, r)) <$> fixity
