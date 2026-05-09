{-# LANGUAGE InstanceSigs #-}

module Interpreter (interpretTerm) where

import Control.Monad (liftM)

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

interpretTerm :: Program -> Term -> Either RuntimeError Value
interpretTerm p t = run (eval t) p

mgu :: Pattern -> Value -> Maybe Substitution
mgu (VarPat x) v = Just $ substitute x (toTerm v)
mgu (ConPat cp ps) (Value cv vs)
    | cp == cv && length ps == length vs = do
        subs <- mapM (\(p, v) -> mgu p v) (zip ps vs)
        Just $ foldl (.) id subs
    | otherwise = Nothing
mgu _ _ = Nothing

{- | p -> v -> term -> term
| Replace toTerm p with toTerm v in t
-}
substitute :: X -> Term -> Term -> Term
substitute v1 t2 t3@(Variable v3)
    | v1 == v3 = t2
    | otherwise = t3
substitute t1 t2 (Constructor c ts) =
    Constructor
        c
        $ map (substitute t1 t2) ts
substitute t1 t2 (Application tl tr) =
    Application
        (substitute t1 t2 tl)
        (substitute t1 t2 tr)
substitute t1 t2 (Case tc cs) = substituteCase t1 t2 (substitute t1 t2 tc) cs
substitute v1 t2 t3@(Function n tf)
    | v1 == n = t3
    | otherwise = Function n $ substitute v1 t2 tf

{- | v1 -> t2 -> tc -> patterns -> Term
| Replace v1 with t2 in patterns
-}
substituteCase :: X -> Term -> Term -> [(Pattern, Term)] -> Term
substituteCase _ _ tc [] = Case tc []
substituteCase v1 t2 tc (c@(pat, tp) : cs)
    | containsVar v1 pat =
        Case tc $ c : (map (\(p, t) -> (p, substitute v1 t2 t)) cs)
    | otherwise =
        Case tc $
            (pat, substitute v1 t2 tp)
                : (map (\(p, t) -> (p, substitute v1 t2 t)) cs)
  where
    containsVar :: X -> Pattern -> Bool
    containsVar x (VarPat v) = x == v
    containsVar x (ConPat _ pts) = any (containsVar x) pts

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
            eval $ substitute x t1 t
        _ ->
            abort $
                "Cannot apply a constructor value to something. Applying  "
                    ++ show v0
                    ++ " to "
                    ++ show t1
eval (Case _ []) = abort $ "Type checker should make sure this never happens"
eval (Case t pts@((_, _) : _)) = do
    v <- eval t
    evalCase v pts

evalCase :: Value -> [(Pattern, Term)] -> Runtime Value
evalCase v [] = abort $ "None of the patterns matched " ++ show v
evalCase v ((pat, term) : pts) = do
    case mgu pat v of
        Nothing -> evalCase v pts
        Just sub -> eval (sub term)

getVar :: X -> Runtime Term
getVar x =
    Runtime
        ( \env -> case gamma env x of
            Nothing -> Left $ "Undefined variable " ++ x
            Just (_, t) -> Right t
        )
