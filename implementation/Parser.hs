-- Justify your changes in the report.

module Parser (parseDefinitions, parseInfixes, parseCommand, parseTerm) where

import Data.Char (isAlphaNum, isControl, isSpace)
import Data.Functor.Identity
import Data.List (groupBy, sortBy)
import Data.Maybe (catMaybes)
import Syntax
import Text.Parsec (parserFail)
import Text.Parsec.Expr
import Text.Parsec.Token (GenTokenParser (reservedOp))
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

parseCommand :: Parser Term -> Prompt -> Either ParseError Command
parseCommand term = parse (command term <* eof) "command"

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
    eval term = Eval <$> term
    help :: Parser Command
    help = strings "?" >> return Help
    quit :: Parser Command
    quit = strings "q" >> return Quit

-- TODO: Probably delete this
parseTerm :: Parser Term -> String -> Either ParseError Term
parseTerm term = parse (term <* eof) "term"

-- TODO: Might want to do the IO in the driver
parseDefinitions :: FilePath -> IO (Either ParseError [Definition])
parseDefinitions f = do
    case parseInfixes f of
        Right term -> parseFromFile (definitions term <* eof) f
        Left p -> return $ Left p
  where
    definitions term = sepBy (definition term) (strings ".")

definition :: Parser Term -> Parser Definition
definition term = dataDef <|> binOpDef <|> varDef
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
        -- TODO: Depending on type inference algorithm, I might need to use
        -- other names here, preferably ones that are not allowed
        return $
            VarDef
                op
                (TypeVar "a" :->: (TypeVar "b" :->: TypeVar "c"))
                (Function x0 (Function x1 t))
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

value :: Parser Term -> Parser Value
value term = constructorValue <|> lambdaValue
  where
    constructorValue = Value <$> constructorName <*> many (value term)
    lambdaValue =
        strings "\\"
            >> Lambda <$> variableName <*> (strings "." >> term)

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

-- Below is the code for the first pass of the parser, which generates the Term
-- parser.

-- | Parse the infix operators
parseInfixes :: String -> Either ParseError (Parser Term)
parseInfixes s = do
    operators <- parse (infixity <* eof) "infixes" s
    let sorted = sortBy sorting operators
        groups = groupBy grouping sorted
        withoutPrec = map (map removePrec) groups
        extractedInfixity = map extractInfixity withoutPrec

    Right $ buildTermParser $ OpTable $ map extractInfixity withoutPrec
  where
    grouping :: (Fixity, Prec, Op) -> (Fixity, Prec, Op) -> Bool
    grouping (f1, p1, _) (f2, p2, _) = f1 == f2 && p1 == p2
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

makeTable :: OpTable -> [[Operator String () Identity Term]]
makeTable (OpTable t) = map mapper t
  where
    mapper :: (Fixity, [Op]) -> [Operator String () Identity Term]
    mapper (_, []) = []
    mapper (f, (o : os)) = binary o (binOpCall o) (fixityToAssoc f) : mapper (f, os)

    binary :: String -> (a -> a -> a) -> Assoc -> Operator String () Identity a
    binary name fun assoc =
        Text.Parsec.Expr.Infix (strings name >> return fun) assoc

    binOpCall :: X -> Term -> Term -> Term
    binOpCall o t1 t2 = Application (Application (Variable o) t1) t2
    fixityToAssoc :: Fixity -> Assoc
    fixityToAssoc Syntax.Infix = AssocNone
    fixityToAssoc InfixL = AssocLeft
    fixityToAssoc InfixR = AssocRight

-- TODO: Add application here.
buildTermParser :: OpTable -> Parser Term
buildTermParser t = buildExpressionParser (makeTable t) literal

literal :: Parser Term
literal =
    functionTerm <|> caseTerm <|> parenTerm <|> Variable
        <$> variableName <|> Constructor
        <$> constructorName
        <*> many term
  where
    functionTerm :: Parser Term
    functionTerm =
        strings "fun"
            >> Function <$> variableName <*> (strings "->" >> term)
    caseTerm :: Parser Term
    caseTerm =
        strings "case"
            >> Case
                <$> term
                <*> ( strings "of"
                        >> many caseInstance
                    )
    parenTerm :: Parser Term
    parenTerm = strings "(" *> term <* strings ")"
    caseInstance = strings ";" >> pair <$> pattern <*> (strings "->" >> term)
    pair l r = (l, r)
    pattern :: Parser Pattern
    pattern = VarPat <$> variableName <|> ConPat <$> constructorName <*> many pattern
