-- Fill in the `undefined` part, feel free to make other changes.
-- Justify your changes in the report.
{-# LANGUAGE InstanceSigs #-}

module TypeChecker (Analysis, checkProgram, checkTerm, runTermCheck) where

import Control.Monad (ap, liftM)
import Syntax

type TypeError = String

type Environment = X -> Either TypeError Type
emptyEnvironment :: Environment
emptyEnvironment x = Left $ "Undefined type variable " ++ x

data Constraint = Type :==: Type

type Substitution = [(X, Type)]

newtype Analysis a
    = Analysis
    {runCheck :: (Program, Environment) -> Either TypeError ([Constraint], a)}

instance Functor Analysis where
    fmap :: (a -> b) -> Analysis a -> Analysis b
    fmap = liftM

instance Applicative Analysis where
    pure :: a -> Analysis a
    pure x = Analysis $ const (Right ([], x))
    (<*>) :: Analysis (a -> b) -> Analysis a -> Analysis b
    (<*>) = ap

instance Monad Analysis where
    (>>=) :: Analysis a -> (a -> Analysis b) -> Analysis b
    a1 >>= a2 =
        Analysis
            ( \(prog, env) ->
                let x1 = runCheck a1 (prog, env)
                    x2 = case x1 of
                        Left e -> Left e
                        Right (cs1, x) -> case runCheck (a2 x) (prog, env) of
                            Left e -> Left e
                            Right (cs2, x') -> Right (cs1 ++ cs2, x')
                 in x2
            )

checkTerm :: Term -> Analysis Type
checkTerm _ = return $ Prim "test" []

runTermCheck :: Term -> Program -> Either TypeError ()
runTermCheck term prog =
    runCheck (checkTerm term) (prog, emptyEnvironment)
        >> return ()

checkProgram :: [Definition] -> Either TypeError Program
checkProgram [] = return $ Program (const Nothing) (const Nothing)
checkProgram ((DataDef d typeVars constructors) : ds) = do
    p <- checkProgram ds
    addDataDef p constructors
  where
    addDataDef :: Program -> [(C, [Type])] -> Either TypeError Program
    addDataDef prog [] = return prog
    addDataDef prog ((c, ts) : cs) =
        addDataDef
            ( prog
                { delta = \y ->
                    if c == y
                        then Just (d, typeVars, ts)
                        else delta prog y
                }
            )
            cs
checkProgram ((VarDef x ty term) : ds) = checkProgram ds >>= addVar
  where
    addVar :: Program -> Either TypeError Program
    addVar prog =
        return $
            prog
                { gamma = \y ->
                    if x == y
                        then Just (ty, term)
                        else gamma prog y
                }

solve :: [Constraint] -> Either TypeError Substitution
solve = undefined
