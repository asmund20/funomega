{-# LANGUAGE InstanceSigs #-}

module TypeChecker (Analysis, checkProgram, runTermCheck, TypeError) where

import Control.Monad (ap, liftM)
import Data.Foldable (foldlM, forM_)
import Data.List (intercalate, sort)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Traversable (forM)
import Syntax

type TypeError = String

type Environment = Map X Type

data Constraint = Type :==: Type deriving (Show)

type Substitution = Map X Type

newtype Analysis a
    = Analysis
    { runCheck ::
        (Program, Environment, Int) ->
        Either TypeError ([Constraint], Substitution, Int, a)
    }

instance Functor Analysis where
    fmap :: (a -> b) -> Analysis a -> Analysis b
    fmap = liftM

instance Applicative Analysis where
    pure :: a -> Analysis a
    pure x = Analysis $ \(_, _, i) -> Right ([], Map.empty, i, x)
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
                        Right (cs1, sub, i', x) -> case runCheck (a2 x) (prog, env, i') of
                            Left e -> Left e
                            Right (cs2, sub', i'', x') ->
                                Right (cs1 ++ cs2, Map.union sub sub', i'', x')
                 in x2

getProgram :: Analysis Program
getProgram = Analysis $ \(prog, _, i) -> Right ([], Map.empty, i, prog)

getEnvironment :: Analysis Environment
getEnvironment = Analysis $ \(_, env, i) -> Right ([], Map.empty, i, env)

localEnv :: Environment -> Analysis a -> Analysis a
localEnv envExtension (Analysis m) = Analysis $ \(prog, env, i) -> m (prog, Map.union envExtension env, i)

isType :: Type -> Type -> Analysis ()
isType t1 t2 = Analysis $ \(_, _, i) -> Right ([t1 :==: t2], Map.empty, i, ())

addChangeBackMapping :: Substitution -> Analysis ()
addChangeBackMapping sub = Analysis $ \(_, _, i) -> Right ([], sub, i, ())

throwError :: TypeError -> Analysis a
throwError e = Analysis $ \(_, _, _) -> Left e

getCounter :: Analysis Int
getCounter = Analysis $ \(_, _, i) -> Right ([], Map.empty, i + 1, i)

newTypeVar :: Analysis Type
newTypeVar = do
    i <- getCounter
    return $ TypeVar $ show i

checkTerm :: Term -> Analysis Type
checkTerm (Variable x) = do
    prog <- getProgram
    env <- getEnvironment
    type' <- case gamma prog x of
        Nothing -> case Map.lookup x env of
            Nothing -> throwError $ "Undefined variable " ++ x
            Just t -> return t
        Just (t, _) -> return t
    return type'
checkTerm (Constructor c terms) = do
    prog <- getProgram
    case delta prog c of
        Nothing -> throwError $ "Undefined constructor " ++ c
        Just (d, xs, types) -> do
            newVarNames <-
                forM xs $ const $ (show <$> getCounter)
            let newTypeVars = map TypeVar newVarNames
            let sub = Map.fromList $ zip xs $ newTypeVars
            receivedTypes <- mapM checkTerm terms
            checkTerms receivedTypes $ map (substituteType sub) types

            addChangeBackMapping $ Map.fromList $ zip newVarNames $ map TypeVar xs

            return $ Prim d newTypeVars
  where
    checkTerms :: [Type] -> [Type] -> Analysis ()
    checkTerms [] [] = return ()
    checkTerms [] _ = throwError $ "Too few arguments to constructor " ++ c
    checkTerms _ [] = throwError $ "Too many arguments to constructor " ++ c
    checkTerms (type1 : type1s) (type2 : type2s) =
        do
            type1 `isType` type2
            checkTerms type1s type2s
checkTerm (Application t0 t1) = do
    type0 <- checkTerm t0
    type1 <- checkTerm t1
    typeVar <- newTypeVar
    type0 `isType` (type1 :->: typeVar)
    return typeVar
checkTerm (Case t cases) = do
    type' <- checkTerm t
    typeCases <- checkCases type' cases
    return typeCases
  where
    checkCases :: Type -> [(Pattern, Term)] -> Analysis Type
    checkCases _ [] = throwError $ "Empty case term"
    checkCases type' cs = do
        types <- forM cs $ checkCase type'
        dummy <- newTypeVar
        foldlM (\t1 t2 -> isType t1 t2 >>= const (return t2)) dummy types
    checkCase :: Type -> (Pattern, Term) -> Analysis Type
    checkCase type' (pat, term) = do
        env <- bindVars pat
        patternType <- localEnv env $ checkTerm $ toTerm pat
        type' `isType` patternType
        termType <- localEnv env $ checkTerm term
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

runTermCheck :: Term -> Program -> Either TypeError Type
runTermCheck term prog = do
    (cs, changeBackSub, _, t) <- runCheck (checkTerm term) (prog, Map.empty, 0)
    sub <- solve cs

    return $ substituteType changeBackSub $ substituteType sub t

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
    localEnv newEnv $ checkVarDef type' term
    checkDefinitions newEnv ds

checkVarDef :: Type -> Term -> Analysis ()
checkVarDef type' term = do
    t <- checkTerm term
    type' `isType` t

checkProgram :: [Definition] -> Either TypeError Program
checkProgram ds =
    if null sameName
        then do
            let p = buildProgram ds
            (cs, changeBackSub, _, _) <-
                runCheck (checkDefinitions Map.empty ds) (p, Map.empty, 0)
            sub <- solve cs
            Right $ buildProgram $ map (substitute changeBackSub) $ map (substitute sub) ds
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

substitute :: Substitution -> Definition -> Definition
substitute _ (d@(DataDef _ _ _)) = d
substitute sub ((VarDef x type' term)) =
    (VarDef x (substituteType sub type') term)

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
    (_, _, sub) <- solve' constraints

    return sub
  where
    solve' ::
        [Constraint] -> Either TypeError ([Constraint], [Constraint], Substitution)
    solve' [] = Right ([], [], Map.empty)
    solve' (c@(t1 :==: t2) : cs)
        | t1 == t2 = solve' cs
        | otherwise = do
            (cs', con, sub) <- resolveSingle c
            (cs'', cons, sub') <- solve' $ cs' ++ substituteConstraints sub cs
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
            Right ([], Just $ TypeVar x :==: tau', Map.singleton x tau')
    resolveSingle (tau :==: (TypeVar x))
        | x `occursIn` tau =
            Left $ "Occurs check failed: " ++ x ++ " in " ++ show tau
        | otherwise = do
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
