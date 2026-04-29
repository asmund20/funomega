-- Justify your changes in the report.
{-# LANGUAGE InstanceSigs #-}

module Interpreter where

import Control.Monad (liftM)
import Debug.Trace (trace, traceM)
import GHC.Base (ap)
import Syntax

type RuntimeError = String

type Substitution = Term -> Term

newtype Runtime a = Runtime {run :: Program -> Either RuntimeError a}

instance Functor Runtime where
    fmap :: (a -> b) -> Runtime a -> Runtime b
    fmap = liftM

instance Applicative Runtime where
    pure :: a -> Runtime a
    pure x = Runtime $ const (Right x)
    (<*>) :: Runtime (a -> b) -> Runtime a -> Runtime b
    (<*>) = ap

instance Monad Runtime where
    return :: a -> Runtime a
    return = pure
    (>>=) :: Runtime a -> (a -> Runtime b) -> Runtime b
    r1 >>= r2 =
        Runtime
            ( \prog ->
                let x1 = run r1 prog
                    x2 = case x1 of
                        Left e -> Left e
                        Right r -> run (r2 r) prog
                 in x2
            )

interpretTerm :: [Definition] -> Term -> Either RuntimeError Value
interpretTerm ds t = run (eval t) (defListToProgram ds)

-- = DataDef D [X] [(C, [Type])] -- A data definition.
-- \| VarDef Name Type Term -- A variable definition.
defListToProgram :: [Definition] -> Program
defListToProgram [] = emptyProgram
defListToProgram ((DataDef d typeVars constructors) : ds) =
    addDataType d typeVars constructors $ defListToProgram ds
  where
    addDataType :: D -> [X] -> [(C, [Type])] -> Program -> Program
    addDataType d typeVars constructors prog =
        prog
            { delta = \y ->
                if d == y
                    then Just (typeVars, constructors)
                    else delta prog y
            }
defListToProgram ((VarDef x ty term) : ds) = addVar x ty term $ defListToProgram ds
  where
    addVar :: X -> Type -> Term -> Program -> Program
    addVar x ty term prog =
        prog
            { gamma = \y ->
                if x == y
                    then Just (ty, term)
                    else gamma prog y
            }

emptyProgram :: Program
emptyProgram = Program (const Nothing) (const Nothing)

mgu :: Pattern -> Value -> Runtime Substitution
mgu = undefined

eval :: Term -> Runtime Value
eval t = do
    traceM "Top of eval term"
    undefined
