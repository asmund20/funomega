-- Do not make any changes in this file, unless you really have to.

module Syntax where

type Prec = Int -- Precedence levels.

type Name = String -- General Names.

type Prompt = String -- A command line input.

type X = Name -- Variable Names.

type C = Name -- Constructor Names.

type D = Name -- Datatype Names.

type Op = Name -- Operator Names.

data Fixity
    = InfixR -- Right-associative.
    | InfixL -- Left-associative.
    | Infix -- Non-associative.
    deriving (Eq, Show)

data Definition
    = DataDef D [X] [(C, [Type])] -- A data definition.
    | VarDef Name Type Term -- A variable definition.
    deriving (Eq, Show)

data OpTable
    = OpTable [(Fixity, [Op])] -- A list of variables, ordered by precedence.
    deriving (Eq, Show)
emptyOpTable :: OpTable
emptyOpTable = OpTable []

data Type
    = Prim D [Type] -- A primitive type.
    | Type :->: Type -- The type arrow.
    | TypeVar X -- A type variable.
    deriving (Eq, Show)

data Pattern
    = VarPat X -- A variable pattern.
    | ConPat C [Pattern] -- A construcor pattern.
    deriving (Eq, Show)

data Term
    = Variable Name -- A variable.
    | Constructor C [Term] -- A constructor.
    | Application Term Term -- A term applied to a term.
    | Case Term [(Pattern, Term)] -- A case-term.
    | Function Name Term -- A function term.
    deriving (Eq, Show)

data Value
    = Value C [Value] -- An algebraic value.
    | Lambda Name Term -- A function value.
    deriving (Eq, Show)

data Command
    = Load FilePath -- Load a program.
    | Reload -- Reload the previous program.
    | Eval Term -- Evaluate this term.
    | Help -- Display a help message.
    | Quit -- Exit the shell.
    deriving (Eq, Show)

data Program
    = Program
    { gamma :: X -> Maybe (Type, Term)
    , delta :: D -> Maybe ([X], [(C, [Type])])
    }

-- note that patterns and values are a subset of term, so you can have:

prettyValue :: Value -> String
prettyValue (Value c []) = '(' : c ++ ")"
prettyValue (Value c vs) = '(' : c ++ " " ++ unwords (map prettyValue vs) ++ ")"
prettyValue (Lambda x t) = "(λ" ++ x ++ "." ++ show t ++ ")"

class IsTerm thing where
    toTerm :: thing -> Term

instance IsTerm Pattern where
    toTerm (VarPat x) = Variable x
    toTerm (ConPat c ps) = Constructor c (toTerm <$> ps)

instance IsTerm Value where
    toTerm (Value c vs) = Constructor c (toTerm <$> vs)
    toTerm (Lambda x t) = Function x t
