{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Types

parser :: Simbol -> UnIndexedTerm
parser (SmAtom "zero")              = TmZero
parser (SmAtom "true")              = TmTrue
parser (SmAtom "false")             = TmFalse
parser (SmAtom s)                   = TmName s
parser (SmList (SmAtom "iszero":t)) = TmApp TmIsZero (parser $ SmList t)
parser (SmList (SmAtom "pred":t))   = TmApp TmPred (parser $ SmList t)
parser (SmList (SmAtom "succ":t))   = TmApp TmSucc (parser $ SmList t)
parser (SmList (SmAtom "if":b:t:f)) = parserIf b t f
parser (SmLambda (SmAtom s) t)      = TmLambda s . parser $ t
parser (SmList [SmLambda v t])      = parser $ SmLambda v t
parser (SmList [SmAtom t])          = parser . SmAtom $ t
parser (SmList [SmList t])          = parser . SmList $ t
parser _                            = NoRuleApplies "Missing sematic analysis"

parserIf :: Simbol -> Simbol -> [Simbol] -> UnIndexedTerm
parserIf b t f = TmApp (TmApp (TmApp TmIf (parser b)) (parser t)) (parser . SmList $ f)

