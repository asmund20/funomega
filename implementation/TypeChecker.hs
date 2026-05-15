{-# LANGUAGE InstanceSigs #-}

module TypeChecker (Analysis, checkProgram, runTermCheck) where

import Control.Monad (ap, liftM)
import Data.Foldable (forM_)
import Data.List (intercalate, sort)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Traversable (forM)
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
    pure x = Analysis $ \(_, _, i) -> Right ([], i, x)
    (<*>) :: Analysis (a -> b) -> Analysis a -> Analysis b
    (<*>) = ap

instance Monad Analysis where
    (>>=) :: Analysis a -> (a -> Analysis b) -> Analysis b
    a1 >>= a2 =
        Analysis $
            \(prog, env, i) ->
                let x1 = runCheck a1 (prog, env, i)
                    x2 = case x1 of
                        Left e -> Left e
                        Right (cs1, i', x) -> case runCheck (a2 x) (prog, env, i') of
                            Left e -> Left e
                            Right (cs2, i'', x') -> Right (cs1 ++ cs2, i'', x')
                 in x2

getProgram :: Analysis Program
getProgram = Analysis $ \(prog, _, i) -> Right ([], i, prog)

getEnvironment :: Analysis Environment
getEnvironment = Analysis $ \(_, env, i) -> Right ([], i, env)

localEnv :: Environment -> Analysis a -> Analysis a
localEnv envExtension (Analysis m) = Analysis $ \(prog, env, i) -> m (prog, Map.union envExtension env, i)

isType :: Type -> Type -> Analysis ()
isType t1 t2 = Analysis $ \(_, _, i) -> Right ([t1 :==: t2], i, ())

throwError :: TypeError -> Analysis a
throwError e = Analysis $ \(_, _, _) -> Left e

newTypeVar :: Analysis Type
newTypeVar = Analysis $ \(_, _, i) -> Right ([], i + 1, TypeVar $ show i)

checkTerm :: Term -> Analysis Type
checkTerm (Variable x) = do
    prog <- getProgram
    env <- getEnvironment
    case gamma prog x of
        Nothing -> case Map.lookup x env of
            Nothing -> throwError $ "Undefined variable " ++ x
            Just t -> return t
        Just (t, _) -> return t
checkTerm (Constructor c terms) = do
    prog <- getProgram
    case delta prog c of
        Nothing -> throwError $ "Undefined constructor " ++ c
        Just (d, xs, types) -> do
            newTypeVars <- forM xs $ const newTypeVar
            let sub = Map.fromList $ zip xs newTypeVars
            receivedTypes <- mapM checkTerm terms
            checkTerms receivedTypes $ map (substituteType sub) types

            return $ Prim d newTypeVars
  where
    checkTerms :: [Type] -> [Type] -> Analysis ()
    checkTerms [] [] = return ()
    checkTerms [] _ = throwError $ "Too few arguments to constructor " ++ c
    checkTerms _ [] = throwError $ "Too many arguments to constructor " ++ c
    checkTerms (type1 : type1s) (type2 : type2s) =
        do
            traceM $
                "Checking constructor arguments: " ++ show type1 ++ " isType " ++ show type2
            type1 `isType` type2
            checkTerms type1s type2s
checkTerm (Application t0 t1) = do
    type0 <- checkTerm t0
    type1 <- checkTerm t1
    typeVar <- newTypeVar
    traceM $
        "Checking application: "
            ++ show (type1 :->: typeVar)
            ++ " isType "
            ++ show type0
    type1 :->: typeVar `isType` type0
    return typeVar
checkTerm (Case t cases) = do
    type' <- checkTerm t
    typeCases <- checkCase type' cases
    return typeCases
  where
    checkCase :: Type -> [(Pattern, Term)] -> Analysis Type
    checkCase _ [] = throwError $ "Empty case term"
    checkCase type' [(pat, term)] = do
        env <- bindVars pat
        patternType <- localEnv env $ checkTerm $ toTerm pat
        traceM $
            "Checking final case (only pattern): "
                ++ show type'
                ++ " isType "
                ++ show patternType
        type' `isType` patternType
        termType <- localEnv env $ checkTerm term
        return termType
    checkCase type' ((pat, term) : rest) = do
        patternType <- checkTerm $ toTerm pat
        traceM $
            "Checking case (pattern): " ++ show type' ++ " isType " ++ show patternType
        type' `isType` patternType
        restType <- checkCase type' rest
        termType <- checkTerm term
        traceM $
            "Checking case (same type as others): "
                ++ show restType
                ++ " isType "
                ++ show termType
        restType `isType` termType
        return termType
    bindVars :: Pattern -> Analysis Environment
    bindVars (VarPat x) = Map.singleton x <$> newTypeVar
    bindVars (ConPat _ pats) = do
        envs <- mapM bindVars pats
        return $ foldl Map.union Map.empty envs
checkTerm (Function x t) = do
    typeVar <- newTypeVar
    retT <- localEnv (Map.singleton x typeVar) $ checkTerm t
    return $ typeVar :->: retT

runTermCheck :: Term -> Program -> Either TypeError ()
runTermCheck term prog = do
    (cs, _, _) <- runCheck (checkTerm term) (prog, Map.empty, 0)
    _ <- solve cs

    return ()

checkDefinitions :: Environment -> [Definition] -> Analysis ()
checkDefinitions _ [] = return ()
checkDefinitions env ((DataDef d xs cons) : ds) = do
    forM_ cons checkCons
    checkDefinitions env ds
  where
    checkCons :: (C, [Type]) -> Analysis ()
    checkCons (_, ts) = forM_ ts checkType
    checkType :: Type -> Analysis ()
    checkType (TypeVar x) =
        if x `elem` xs
            then return ()
            else
                throwError $
                    "Undefined type variable "
                        ++ x
                        ++ " in data definition "
                        ++ d
    checkType (Prim d' ts) = do
        prog <- getProgram
        if datas prog d'
            then forM_ ts checkType
            else throwError $ "Undefined data: " ++ d'
    checkType (t0 :->: t1) = do
        checkType t0
        checkType t1
checkDefinitions env ((VarDef x type' term) : ds) = do
    let newEnv = Map.insert x type' env
    traceM $ "Checking type for " ++ x ++ ": " ++ show type' ++ " = " ++ show term
    localEnv newEnv $ checkVarDef type' term
    traceM "\n\n\n"
    checkDefinitions newEnv ds

checkVarDef :: Type -> Term -> Analysis ()
checkVarDef type' term = do
    t <- checkTerm term
    traceM $
        "Checking var def for "
            ++ show term
            ++ " (declared type and inferred type): "
            ++ show type'
            ++ " isType "
            ++ show t
    type' `isType` t

checkProgram :: [Definition] -> Either TypeError Program
checkProgram ds =
    if null sameName
        then do
            let p = buildProgram ds
            (cs, _, _) <- runCheck (checkDefinitions Map.empty ds) (p, Map.empty, 0)
            traceM $ show cs
            sub <- solve cs
            traceM $ show sub
            Right $ buildProgram $ substitute sub ds
        else do
            Left $
                "Found top-level definitions with the same name. Multiple instances of "
                    ++ intercalate ", " sameName
  where
    sameName :: [Name]
    sameName =
        let constructorNames = concatMap getConstructorNames ds
            defNames = map getName ds
            groupedNames = NonEmpty.group $ sort $ constructorNames ++ defNames
            duplicates = catMaybes $ map longerThanOne groupedNames
         in map NonEmpty.head duplicates
    getName :: Definition -> Name
    getName (VarDef x _ _) = x
    getName (DataDef d _ _) = d
    getConstructorNames :: Definition -> [Name]
    getConstructorNames (VarDef _ _ _) = []
    getConstructorNames (DataDef _ _ cs) = map (\(c, _) -> c) cs
    longerThanOne :: NonEmpty a -> Maybe (NonEmpty a)
    longerThanOne (_ :| []) = Nothing
    longerThanOne l = Just l

substitute :: Substitution -> [Definition] -> [Definition]
substitute _ [] = []
substitute sub (d@(DataDef _ _ _) : ds) = d : substitute sub ds
substitute sub ((VarDef x type' term) : ds) =
    (VarDef x (Map.findWithDefault type' x sub) term) : substitute sub ds

buildProgram :: [Definition] -> Program
buildProgram [] = Program (const Nothing) (const Nothing) $ const False
buildProgram ((DataDef d typeVars constructors) : ds) =
    let prog = addConstructors (buildProgram ds) constructors
     in prog
            { datas = \y ->
                if y == d then True else datas prog y
            }
  where
    addConstructors :: Program -> [(C, [Type])] -> Program
    addConstructors prog [] = prog
    addConstructors prog ((c, ts) : cs) =
        addConstructors
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
solve constraints = do
    (_, solvedCs, sub) <- solve' constraints
    traceM $ "Solved constraints, result is:\n" ++ show solvedCs

    return sub
  where
    solve' ::
        [Constraint] -> Either TypeError ([Constraint], [Constraint], Substitution)
    solve' [] = Right ([], [], Map.empty)
    solve' (c@(t1 :==: t2) : cs)
        | t1 == t2 = solve' cs
        | otherwise = do
            (cs', con, sub) <- resolveSingle c
            traceM $ "Got substitution " ++ show sub
            let subbed = cs' ++ substituteConstraints sub cs
            traceM $ "After substitution, constraints are " ++ show subbed
            (cs'', cons, sub') <- solve' $ cs' ++ subbed
            let sub'' = Map.union sub sub'
            return
                ( cs''
                , substituteConstraints sub'' $ con `appendIfJust` cons
                , sub''
                )
    appendIfJust :: Maybe a -> [a] -> [a]
    appendIfJust ma as = case ma of
        Nothing -> as
        Just a -> a : as
    resolveSingle ::
        Constraint -> Either TypeError ([Constraint], Maybe Constraint, Substitution)
    resolveSingle ((tau1 :->: tau2) :==: (tau1' :->: tau2')) =
        Right ([tau1 :==: tau1', tau2 :==: tau2'], Nothing, Map.empty)
    resolveSingle (tau1@(Prim d ts) :==: tau2@(Prim d' ts'))
        | d == d' && length ts == length ts' =
            Right
                ( map (\(tau, tau') -> tau :==: tau') (zip ts ts')
                , Nothing
                , Map.empty
                )
        | otherwise =
            Left $ "Primitive type mismatch: " ++ show tau1 ++ " and " ++ show tau2
    resolveSingle ((TypeVar x) :==: tau')
        | x `occursIn` tau' =
            Left $ "Occurs check failed: " ++ x ++ " in " ++ show tau'
        | otherwise = do
            traceM $ "Solved " ++ x ++ ", it is: " ++ show tau'
            Right ([], Just $ TypeVar x :==: tau', Map.singleton x tau')
    resolveSingle (tau :==: (TypeVar x))
        | x `occursIn` tau =
            Left $ "Occurs check failed: " ++ x ++ " in " ++ show tau
        | otherwise = do
            traceM $ "Solved " ++ x ++ ", it is: " ++ show tau
            Right ([], Just $ TypeVar x :==: tau, Map.singleton x tau)
    resolveSingle (tau1 :==: tau2) =
        Left $
            "Type mismatch between primitive and function: "
                ++ show tau1
                ++ " and "
                ++ show tau2
    occursIn :: X -> Type -> Bool
    occursIn x (TypeVar y) = y == x
    occursIn x (tau1 :->: tau2) = x `occursIn` tau1 || x `occursIn` tau2
    occursIn x (Prim _ ts) = any (x `occursIn`) ts

substituteConstraints :: Substitution -> [Constraint] -> [Constraint]
substituteConstraints sub cs =
    map
        ( \(type1 :==: type2) ->
            substituteType sub type1 :==: substituteType sub type2
        )
        cs

substituteType :: Substitution -> Type -> Type
substituteType sub t@(TypeVar x) = Map.findWithDefault t x sub
substituteType sub (type1 :->: type2) =
    substituteType sub type1 :->: substituteType sub type2
substituteType sub (Prim d ts) = Prim d $ map (substituteType sub) ts
