-- Fill in the `undefined` part, feel free to make other changes.
-- Justify your changes in the report.

module TypeChecker where

import Syntax

type TypeError = String

type Environment = X -> Either TypeError Type

data Constraint = Type :==: Type

type Substitution = [(X, Type)]

newtype Analysis a
    = Analysis
    {runCheck :: (Program, Environment) -> Either TypeError ([Constraint], a)}

checkTerm :: Term -> Analysis Type
checkTerm = undefined

checkProgram :: [Definition] -> Analysis Program
checkProgram = undefined

solve :: [Constraint] -> Either TypeError Substitution
solve = undefined
