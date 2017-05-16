{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Types

parser :: Simbol -> UnIndexedTerm
parser simbol = case simbol of
  (SmAtom "zero")                    -> TmZero
  (SmAtom "true")                    -> TmTrue
  (SmAtom "false")                   -> TmFalse
  (SmAtom s)                         -> TmName s
  (SmLambda(SmAtom s)t1)             -> TmLambda s . parser $ t1
  (SmList [SmLambda v t])            -> parser $ SmLambda v t
  (SmList [SmAtom t])                -> parser . SmAtom $ t
  (SmList [SmList t])                -> parser . SmList $ t
  (SmList (SmAtom "iszero":t))       -> TmApp TmIsZero (parser $ SmList t)
  (SmList (SmAtom "pred":t))         -> TmApp TmPred (parser $ SmList t)
  (SmList (SmAtom "succ":t))         -> TmApp TmSucc (parser $ SmList t)
  (SmList (SmAtom "if":b:t:f))       -> parserIf b t f
  (SmList (t1:t2))                   -> TmApp (parser t1) $ parser (SmList t2)
  _                                  -> NoRuleApplies "Missing sematic analysis"

parserIf :: Simbol -> Simbol -> [Simbol] -> UnIndexedTerm
parserIf b t f = TmApp (TmApp (TmApp TmIf (parser b)) (parser t)) (parser . SmList $ f)

