-- Justify your changes in the report.
{-# LANGUAGE InstanceSigs #-}

module Interpreter () where

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

abort :: RuntimeError -> Runtime a
abort re = Runtime (const (Left re))

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

mgu :: Pattern -> Value -> Maybe Substitution
mgu (VarPat x) v = Just $ substitute x v
mgu p@(ConPat cp ps) v@(Value cv vs)
    | cp == cv && (toTerm p) == (toTerm v) = do
        subs <- mapM (\(p, v) -> mgu p v) (zip ps vs)
        Just $ foldl (.) (\x -> x) subs
    | otherwise = Nothing
mgu _ _ = Nothing

{- | p -> v -> term -> term
| Replace toTerm p with toTerm v in t
-}
substitute :: X -> Value -> Term -> Term
substitute x v = substitute' (Variable x) (toTerm v)
  where
    -- Replace t1 with t2 in t3, t1 can only be variable
    substitute' :: Term -> Term -> Term -> Term
    substitute t1 t2 t3@(Variable _)
        | t1 == t3 = t2
        | otherwise = t3
    substitute' t1 t2 (Constructor c ts) =
        Constructor
            c
            $ map (substitute' t1 t2) ts
    substitute' t1 t2 (Application tl tr) =
        Application
            (substitute' t1 t2 tl)
            (substitute' t1 t2 tr)
    substitute' t1 t2 (Case tc cs) = substituteCase t1 t2 $ Case (substitute' t1 t2 tc) cs
    substitute' t1@(Variable x) t2 t3@(Function n tf)
        | x == n = t3
        | otherwise = Function n $ substitute' t1 t2 tf
    substitute' t1 t2 (Function n tf) = Function n $ substitute' t1 t2 tf
    substituteCase t1 t2 t3@(Case _ []) = t3
    substituteCase t1@(Variable v1) t2 t3@(Case tc (c@((VarPat vc), tp) : cs))
        | v1 == vc = Case tc $ c : (map (\(p, t) -> (p, substitute' t1 t2 t)) cs)
        | otherwise =
            Case tc $
                ((VarPat vc), substitute' t1 t2 tp)
                    : (map (\(p, t) -> (p, substitute' t1 t2 t)) cs)

eval :: Term -> Runtime Value
eval (Variable x) = getVar x >>= eval
eval (Constructor c ts) = do
    vals <- mapM eval ts
    return $ Value c vals
eval (Function x t) = return $ Lambda x t
eval (Application t0 t1) = do
    v0 <- eval t0
    case v0 of
        Lambda x t -> do
            v1 <- eval t1
            replace x (toTerm v1) t >>= eval
        _ -> abort "Cannot apply a constructor value to something"
eval (Case t []) = abort $ "Type checker should make sure this never happens"
eval (Case t pts@((_, _) : _)) = do
    v <- eval t
    evalCase v pts
  where
    evalCase :: Value -> [(Pattern, Term)] -> Runtime Value
    evalCase _ [] = abort "None of the patterns matched"
    evalCase v ((pat, term) : pts) = do
        mSub <- mgu pat v
        case mSub of
            Nothing -> evalCase v pts
            Just sub -> eval (sub term)

replace :: X -> Term -> Term -> Runtime Term
replace x t (Variable x')
    | x' == x = return t
    | otherwise = return $ Variable x'
replace x t (Constructor c ts) = do
    vs <- mapM (replace x t) ts
    return $ Constructor c vs
replace x t (Function x' t')
    | x == x' = return $ Function x' t'
    | otherwise = replace x t t'
replace x t (Application t1 t2) = do
    t1' <- replace x t t1
    t2' <- replace x t t2
    return $ Application t1' t2'
replace x t (Case tc pts) = do
    tc' <- replace x t tc
    return $ Case tc' pts

getVar :: X -> Runtime Term
getVar x =
    Runtime
        ( \env -> case gamma env x of
            Nothing -> Left $ "Undefined variable " ++ x
            Just (_, t) -> Right t
        )
