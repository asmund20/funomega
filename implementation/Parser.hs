-- Fill in the `undefined` part, feel free to make other changes.
-- Justify your changes in the report.

module Parser where

import Data.Char (isAlphaNum, isControl, isSpace)
import Data.List (groupBy, sortBy)
import Syntax
import Text.ParserCombinators.Parsec

parseInfixes :: String -> Either ParseError OpTable
parseInfixes s = do
    operators <- parse (infixity <* eof) "infixes" s
    let groups = groupBy grouping operators
        sortedGroups = map (sortBy sorting) groups
        withoutPrec = map (map removePrec) sortedGroups
        extractedInfixity = map extractInfixity withoutPrec

    return $ OpTable $ map extractInfixity withoutPrec
  where
    grouping a b = a == b
    sorting (_, l, _) (_, r, _) = compare l r
    removePrec (f, _, n) = (f, n)
    extractInfixity l@((i, _) : _) = (i, map second l)
    second (_, v) = v

infixity :: Parser [(Fixity, Prec, Op)]
infixity = otherDef >> sepEndBy binOpDef otherDef

otherDef :: Parser ()
otherDef = optional $ manyTill anyChar $ char '.' >> spaces

fixity :: Parser (Prec -> Op -> (Fixity, Prec, Op))
fixity =
    string "infix"
        >> ( parseInfix
                <|> parseInfixL
                <|> parseInfixR
           )
            <* spaces
  where
    parseInfix :: Parser (Prec -> Op -> (Fixity, Prec, Op))
    parseInfix = space >> return (triple Infix)
    parseInfixL :: Parser (Prec -> Op -> (Fixity, Prec, Op))
    parseInfixL = char 'l' >> space >> return (triple InfixL)
    parseInfixR :: Parser (Prec -> Op -> (Fixity, Prec, Op))
    parseInfixR = char 'r' >> space >> return (triple InfixL)
    triple :: Fixity -> Prec -> Op -> (Fixity, Prec, Op)
    triple fst snd trd = (fst, snd, trd)

binOpDef :: Parser (Fixity, Prec, Op)
binOpDef =
    fixity
        <*> (nat <* manyTill alphaNum space <* spaces)
        <*> binOpName
        <* manyTill anyChar (char '.')
        <* spaces

nat :: Parser Int
nat = zero <|> nonzero
  where
    zero = char '0' >> return 0 <* spaces
    nonzero = read <$> many1 digit <* spaces

binOpName :: Parser (Op)
binOpName = many1 (satisfy binOpChar) <* spaces
  where
    binOpChar c =
        (not $ isControl c)
            && (not $ isAlphaNum c)
            && (not $ isSpace c)

parseCommand :: Prompt -> Either ParseError Command
parseCommand = undefined

parseTerm :: String -> Either ParseError Term
parseTerm = undefined

parseDefinitions :: FilePath -> Either ParseError [Definition]
parseDefinitions = undefined
