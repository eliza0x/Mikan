{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Types

parser :: Simbol -> UnIndexedTerm
parser simbol = case simbol of
 (SmAtom "zero")        -> TmZero
 (SmAtom "iszero")      -> TmIsZero
 (SmAtom "pred")        -> TmPred
 (SmAtom "succ")        -> TmSucc
 (SmAtom s)             -> TmName s
 (SmLambda(SmAtom s)t1) -> TmLambda s $ parser t1
 (SmList [t])           -> parser t
 (SmList ts)            -> TmApp (parser . SmList $ init ts) (parser $ last ts)
 _                      -> NoRuleApplies "Missing sematic analysis"
