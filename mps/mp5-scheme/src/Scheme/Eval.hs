{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Scheme.Eval where

import Scheme.Core

import Prelude hiding (lookup)
import qualified Data.HashMap.Strict as H (HashMap, insert, lookup, empty, fromList, union)
import Control.Monad.State
import Control.Monad.Except
import Debug.Trace

-- ### Evaluation helpers

-- Evaluates a symbol to string
-- Throws an error if value is not a symbol
-- Examples:
--   getSym (Symbol "x")  ==> "x"
--   getSym (Number 1)    ==> Not a symbol: x
getSym :: Val -> EvalState String
getSym (Symbol x) = return x
getSym         v  = throwError $ NotASymbol v

-- `let` and `let*`
getBinding :: Val -> EvalState (String, Val)
getBinding (Pair c (Pair e Nil)) = liftM2 (,) (getSym c) (eval e)
getBinding v = throwError $ NotAListOfTwo v

-- Evaluates a list of two to a tuple
-- Throws an error if value is not a list of two
-- This is useful in special form `cond`, since each clause
-- is expected to be exactly a two-element list
getListOf2 :: Val -> EvalState (Val, Val)
getListOf2 (Pair c (Pair e Nil)) = return (c, e)
getListOf2 v = throwError $ NotAListOfTwo v

-- Evaluates a value representing a list into an actual list
getList :: Val -> EvalState [Val]
getList Nil = return []
getList (Pair v1 v2) =
  do xs <- getList v2
     return (v1 : xs)
getList e = throwError $ InvalidSpecialForm "special" e

--- ### Keywords

-- When evaluating special forms, a list form starting with a keyword
-- is expected to match the special form syntax.
keywords :: [String]
keywords = [ "define"
           , "lambda"
           , "cond"
           , "let"
           , "let*"
           , "define-macro"
           , "quasiquote"
           , "unquote"
           ]

-- ### The monadic evaluator
-- Unlike evaluators in previous MPs, `eval` does not take any environment!
-- This is because the environment is encapsulated in the `EvalState` monad.
-- To access the environment, all you have to do is `get`, `modify` or `put`!
eval :: Val -> EvalState Val

-- Self-evaluating expressions
-- TODO: What's self-evaluating?
eval v@(Number _) = unimplemented "Evaluating numbers"
eval v@(Boolean _) = unimplemented "Evaluating booleans"

-- Symbol evaluates to the value bound to it
-- TODO
eval (Symbol sym) =
  do env <- get
     case H.lookup sym env of
       Nothing -> throwError $ UndefSymbolError sym
       Just val -> return val

-- Function closure is also self-evaluating
eval v@(Func _ _ _) = return v

-- We check to see if the pair is a "proper list". If it is,
-- then we try to evaluate it, as one of the following forms:
-- 1. Special form (`define`, `let`, `let*`, `cond`, `quote`, `quasiquote`,
--    `unquote`, `define-macro`, ...)
-- 2. Macro expansion (Macro)
-- 3. Function application (Func)
-- 4. Primitive function application (PrimFunc)
eval expr@(Pair v1 v2) = case flattenList expr of
  Left _ -> throwError $ InvalidExpression expr
  Right lst -> evalList lst where
    --- Evaluator for forms
    invalidSpecialForm :: String -> EvalState e
    invalidSpecialForm frm = throwError $ InvalidSpecialForm frm expr

    evalList :: [Val] -> EvalState Val

    evalList [] = throwError $ InvalidExpression expr

    -- quote
    -- TODO
    evalList [Symbol "quote", e] = return e

    -- unquote (illegal at surface evaluation)
    -- TODO: since surface-level `unquote` is illegal, all you need to do is
    -- to throw a diagnostic
    evalList [Symbol "unquote", e] = throwError $ UnquoteNotInQuasiquote e

    -- quasiquote
    evalList [Symbol "quasiquote", e] = evalQuasi 1 e where
      evalQuasi :: Int -> Val -> EvalState Val
      evalQuasi 0 (Pair (Symbol "unquote") v) = throwError $ UnquoteNotInQuasiquote v
      evalQuasi 1 (Pair (Symbol "unquote") (Pair v Nil)) = eval v
      evalQuasi n (Pair (Symbol "quasiquote") (Pair v Nil)) =
        do v' <- evalQuasi (n+1) v
           return $ Pair (Symbol "quasiquote") (Pair v' Nil)
      evalQuasi n (Pair (Symbol "unquote") (Pair v Nil)) =
        do v' <- evalQuasi (n-1) v
           return $ Pair (Symbol "unquote") (Pair v' Nil)
      evalQuasi n (Pair x y) = Pair <$> evalQuasi n x <*> evalQuasi n y
      evalQuasi _ v = return v

    -- cond
    -- TODO: Handle `cond` here. Use pattern matching to match the syntax

    -- let
    -- TODO: Handle `let` here. Use pattern matching to match the syntax

    -- lambda
    -- TODO: Handle `lambda` here. Use pattern matching to match the syntax


    -- define function
    evalList [Symbol "define", Pair (Symbol fname) args, body] =
      do env <- get
         argList <- getList args
         val <- (\argVal -> Func argVal body env) <$> mapM getSym argList
         modify $ H.insert fname val
         trace ("fname\n" ++ show fname) $ trace ("args!!!!\n" ++ show fname) $ return Void

    -- define variable
    -- TODO: Handle `define` for variables here. Use pattern matching
    -- to match the syntax
    evalList [Symbol "define", (Symbol x), varexp] =
      do env <- get
         val <- eval varexp
         modify $ H.insert x val
         trace ("ENV!!\n" ++ show env) $ trace ("val!!\n" ++ show val) $ trace ("x!!\n" ++ show x) $ return Void
    -- define-macro
    -- TODO: Handle `define-macro` here. Use pattern matching to match
    -- the syntax
    evalList [Symbol "define-macro", Pair (Symbol fname) args, expr] =
      do env <- get
         argList <- getList args
         val <- (\argVal -> Func argVal expr env) <$> mapM getSym argList
         modify $ H.insert fname val
         return Void

    -- invalid use of keyword, throw a diagnostic
    evalList (Symbol sym : _) | elem sym keywords = invalidSpecialForm sym

    -- application
    evalList (fexpr:args) =
      do f <- eval fexpr
         apply f args

eval val = throwError $ InvalidExpression val

-- Function application
apply :: Val -> [Val] -> EvalState Val
  -- Function
    -- TODO: implement function application
    -- Use do-notation!
apply (Func fmls body cenv) args | length fmls == length args = 
  do env <- get
     modify (H.union cenv)
     mapM_ (\(k,v)-> modify (H.insert k v)) (zip fmls args)
     val <- eval body
     put env
     return val

  -- Macro
    -- TODO: implement macro evaluation
    -- Use do-notation!

  -- Primitive
apply (PrimFunc p) args =
  do argVals <- mapM eval args
     p argVals
  -- Other values are not applicable
apply f args = throwError $ CannotApply f args
