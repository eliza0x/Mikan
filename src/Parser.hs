{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Types
-- import Control.Monad.State

parser :: Simbol -> Term
parser (SmAtom "zero")              = TmZero
parser (SmAtom "true")              = TmTrue
parser (SmAtom "false")             = TmFalse
parser (SmList (SmAtom "iszero":t)) = TmIsZero . parser . SmList $ t
parser (SmList (SmAtom "pred":t))   = TmPred . parser . SmList $ t
parser (SmList (SmAtom "succ":t))   = TmSucc . parser . SmList $ t
parser (SmList (SmAtom "if":b:t:f)) = TmIf (parser b) (parser t) (parser . SmList $ f)
parser (SmList [SmList t])          = parser . SmList $ t
parser (SmList [SmAtom t])          = parser . SmAtom $ t
parser _                            = NoRuleApplies "Missing sematic analysis"

