{-# LANGUAGE InstanceSigs #-}

module TypeChecker (Analysis, checkProgram, checkTerm, runTermCheck) where

import Control.Monad (ap, liftM)
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace (traceM)
import Syntax

type TypeError = String

type Environment = X -> Maybe Type

emptyEnvironment :: Environment
emptyEnvironment _ = Nothing

data Constraint = Type :==: Type

type Substitution = Map X Type

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

getProgram :: Analysis Program
getProgram = Analysis $ \(prog, _) -> Right ([], prog)

getEnvironment :: Analysis Environment
getEnvironment = Analysis $ \(_, env) -> Right ([], env)

localEnv :: X -> Type -> Analysis a -> Analysis a
localEnv x t (Analysis m) = Analysis $ \(prog, env) -> m (prog, f env)
  where
    f :: Environment -> Environment
    f env = \y ->
        if x == y
            then Just t
            else env y

isType :: Type -> Type -> Analysis ()
isType t1 t2 = Analysis $ \(_, _) -> Right ([t1 :==: t2], ())

throwError :: TypeError -> Analysis a
throwError e = Analysis $ \(_, _) -> Left e

-- TODO
newTypeVar :: Analysis Type
newTypeVar = do
    traceM "newTypeVar is undefined"
    return $ TypeVar "dummy"

checkTerm :: Term -> Analysis Type
checkTerm (Variable x) = do
    env <- getEnvironment
    case env x of
        Nothing -> newTypeVar
        Just t -> return t
checkTerm (Constructor c terms) = do
    prog <- getProgram
    case delta prog c of
        Nothing -> throwError $ "Undefined constructor " ++ c
        Just (d, _, types) -> do
            receivedTypes <- mapM checkTerm terms
            checkTerms receivedTypes types

            return $ Prim d receivedTypes
  where
    checkTerms :: [Type] -> [Type] -> Analysis ()
    checkTerms [] [] = return ()
    checkTerms [] _ = throwError $ "Too few arguments to constructor " ++ c
    checkTerms _ [] = throwError $ "Too many arguments to constructor " ++ c
    checkTerms (type1 : type1s) (type2 : type2s) =
        type1 `isType` type2
            >>= const (checkTerms type1s type2s)
checkTerm (Application t0 t1) = do
    type0 <- checkTerm t0
    type1 <- checkTerm t1
    typeVar <- newTypeVar
    type1 :->: typeVar `isType` type0
    return type0
checkTerm (Case t cases) = do
    type' <- checkTerm t
    typeCases <- checkCase type' cases
    return typeCases
  where
    checkCase :: Type -> [(Pattern, Term)] -> Analysis Type
    checkCase _ [] = newTypeVar
    checkCase type' ((pat, term) : rest) = do
        patternType <- checkTerm $ toTerm pat
        type' `isType` patternType
        restType <- checkCase type' rest
        termType <- checkTerm term
        restType `isType` termType
        return termType
checkTerm (Function x t) = do
    typeVar <- newTypeVar
    retT <- localEnv x typeVar (checkTerm t)
    return $ typeVar :->: retT

runTermCheck :: Term -> Program -> Either TypeError ()
runTermCheck term prog =
    runCheck (checkTerm term) (prog, emptyEnvironment)
        >> return ()

checkDefinitions :: [Definition] -> Analysis ()
checkDefinitions [] = return ()
checkDefinitions ((DataDef _ _ _) : ds) = checkDefinitions ds
checkDefinitions ((VarDef _ type' term) : ds) = do
    checkVarDef type' term
    checkDefinitions ds

checkVarDef :: Type -> Term -> Analysis ()
checkVarDef type' term = do
    t <- checkTerm term
    type' `isType` t

checkProgram :: [Definition] -> Either TypeError Program
checkProgram ds = do
    -- TODO: Could I use an empty program here?
    let p = buildProgram ds
    (cs, _) <- runCheck (checkDefinitions ds) (p, emptyEnvironment)
    sub <- solve cs
    Right $ buildProgram $ substitute sub ds

substitute :: Substitution -> [Definition] -> [Definition]
substitute _ [] = []
substitute sub (d@(DataDef _ _ _) : ds) = d : substitute sub ds
substitute sub ((VarDef x type' term) : ds) =
    (VarDef x (Map.findWithDefault type' x sub) term) : substitute sub ds

buildProgram :: [Definition] -> Program
buildProgram [] = Program (const Nothing) (const Nothing)
buildProgram ((DataDef d typeVars constructors) : ds) =
    addDataDef (buildProgram ds) constructors
  where
    addDataDef :: Program -> [(C, [Type])] -> Program
    addDataDef prog [] = prog
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
buildProgram ((VarDef x ty term) : ds) = addVar $ buildProgram ds
  where
    addVar :: Program -> Program
    addVar prog =
        prog
            { gamma = \y ->
                if x == y
                    then Just (ty, term)
                    else gamma prog y
            }

-- TODO
solve :: [Constraint] -> Either TypeError Substitution
solve _ = do
    traceM "Constraint solving is undefined"
    Right Map.empty
