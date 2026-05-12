{-# LANGUAGE InstanceSigs #-}

module TypeChecker (Analysis, checkProgram, checkTerm, runTermCheck) where

import Control.Monad (ap, liftM)
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace (traceM)
import Syntax

type TypeError = String

type Environment = Map X Type

data Constraint = Type :==: Type deriving (Show)

type Substitution = Map X Type

newtype Analysis a
    = Analysis
    { runCheck ::
        (Program, Environment, Int) -> Either TypeError ([Constraint], Int, a)
    }

instance Functor Analysis where
    fmap :: (a -> b) -> Analysis a -> Analysis b
    fmap = liftM

instance Applicative Analysis where
    pure :: a -> Analysis a
    pure x = Analysis $ \(_, _, i) -> (Right ([], i, x))
    (<*>) :: Analysis (a -> b) -> Analysis a -> Analysis b
    (<*>) = ap

instance Monad Analysis where
    (>>=) :: Analysis a -> (a -> Analysis b) -> Analysis b
    a1 >>= a2 =
        Analysis
            ( \(prog, env, i) ->
                let x1 = runCheck a1 (prog, env, i)
                    x2 = case x1 of
                        Left e -> Left e
                        Right (cs1, i', x) -> case runCheck (a2 x) (prog, env, i') of
                            Left e -> Left e
                            Right (cs2, i'', x') -> Right (cs1 ++ cs2, i'', x')
                 in x2
            )

getProgram :: Analysis Program
getProgram = Analysis $ \(prog, _, i) -> Right ([], i, prog)

getEnvironment :: Analysis Environment
getEnvironment = Analysis $ \(_, env, i) -> Right ([], i, env)

localEnv :: X -> Type -> Analysis a -> Analysis a
localEnv x t (Analysis m) = Analysis $ \(prog, env, i) -> m (prog, Map.insert x t env, i)

isType :: Type -> Type -> Analysis ()
isType t1 t2 = Analysis $ \(_, _, i) -> Right ([t1 :==: t2], i, ())

throwError :: TypeError -> Analysis a
throwError e = Analysis $ \(_, _, _) -> Left e

newTypeVar :: Analysis Type
newTypeVar = Analysis $ \(_, _, i) -> Right ([], i + 1, TypeVar $ show i)

-- newTypeVar :: Analysis Type
-- newTypeVar = do
--     i <- newVar
--     traceM $ "Generating new type var " ++ show i
--     return i
--   where
--     newVar = Analysis $ \(_, _, i) -> Right ([], i + 1, TypeVar $ show i)

checkTerm :: Term -> Analysis Type
checkTerm (Variable x) = do
    env <- getEnvironment
    case Map.lookup x env of
        Nothing -> newTypeVar
        Just t -> return t
checkTerm (Constructor c terms) = do
    prog <- getProgram
    case delta prog c of
        Nothing -> throwError $ "Undefined constructor " ++ c
        Just (d, xs, types) -> do
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
    runCheck (checkTerm term) (prog, Map.empty, 0)
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
    (cs, _, _) <- runCheck (checkDefinitions ds) (p, Map.empty, 0)
    traceM $ show cs
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

solve :: [Constraint] -> Either TypeError Substitution
solve cs = do
    (_, sub) <- resolveConstraints cs
    return sub

resolveConstraints ::
    [Constraint] -> Either TypeError ([Constraint], Substitution)
resolveConstraints [] = Right ([], Map.empty)
resolveConstraints (c@(t1 :==: t2) : cs)
    | t1 == t2 = resolveConstraints cs
    | otherwise = do
        (cs', sub) <- resolveSingle c
        resolveConstraints (cs' ++ substituteConstraints sub cs)
  where
    resolveSingle :: Constraint -> Either TypeError ([Constraint], Substitution)
    resolveSingle ((tau1 :->: tau2) :==: (tau1' :->: tau2')) =
        Right ([tau1 :==: tau1', tau2 :==: tau2'], Map.empty)
    resolveSingle (tau1@(Prim d ts) :==: tau2@(Prim d' ts'))
        | d == d' && length ts == length ts' =
            Right (map (\(tau, tau') -> tau :==: tau') (zip ts ts'), Map.empty)
        | otherwise =
            Left $ "Primitive type mismatch: " ++ show tau1 ++ " and " ++ show tau2
    resolveSingle ((TypeVar x) :==: tau')
        | x `occursIn` tau' =
            Left $ "Occurs check failed: " ++ x ++ " in " ++ show tau'
        | otherwise = Right ([], Map.singleton x tau')
    resolveSingle (tau :==: (TypeVar x))
        | x `occursIn` tau =
            Left $ "Occurs check failed: " ++ x ++ " in " ++ show tau
        | otherwise = Right ([], Map.singleton x tau)
    resolveSingle (tau1 :==: tau2) =
        Left $ "Type mismatch: " ++ show tau1 ++ " and " ++ show tau2
    substituteConstraints :: Substitution -> [Constraint] -> [Constraint]
    substituteConstraints _ [] = []
    substituteConstraints sub ((type1 :==: type2) : cs') =
        substituteType sub type1 :==: substituteType sub type2 : cs'
    substituteType :: Substitution -> Type -> Type
    substituteType sub t@(TypeVar x) = Map.findWithDefault t x sub
    substituteType sub (type1 :->: type2) =
        substituteType sub type1 :->: substituteType sub type2
    substituteType sub (Prim d ts) = Prim d (map (substituteType sub) ts)
    occursIn :: X -> Type -> Bool
    occursIn x (TypeVar y) = y == x
    occursIn x (tau1 :->: tau2) = x `occursIn` tau1 || x `occursIn` tau2
    occursIn x (Prim _ ts) = any (x `occursIn`) ts
