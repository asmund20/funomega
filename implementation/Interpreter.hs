-- Fill in the `undefined` part, feel free to make other changes.
-- Justify your changes in the report.

module Interpreter where

import Syntax

type    RuntimeError = String
type    Substitution = Term -> Term
newtype Runtime a    = Runtime { run :: Program -> Either RuntimeError a }

emptyProgram :: Program
emptyProgram = Program (const Nothing) (const Nothing)

mgu :: Pattern -> Value -> Runtime Substitution
mgu = undefined

eval :: Term -> Runtime Value
eval = undefined
