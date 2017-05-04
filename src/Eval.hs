module Eval(eval) where

import Types

eval :: Term -> Term
eval t = if t == t' then t' else eval t' where
  t' = eval1 t

eval1 :: Term -> Term
eval1 t = case t of
  TmZero                   -> TmZero
  TmTrue                   -> TmTrue
  TmFalse                  -> TmFalse
  TmIf TmTrue  t' _        -> t'
  TmIf TmFalse _  t'       -> t'
  TmIf t' iftrue  iffalse  -> if not $ isnumerical t then TmIf (eval t') iftrue iffalse 
                                                      else NoRuleApplies
  TmSucc t'                -> TmSucc $ if isnumerical t' then t' else NoRuleApplies
  TmPred TmZero            -> TmZero
  TmPred (TmSucc t')       -> if isnumerical t' then t' else NoRuleApplies
  TmPred t'                -> TmPred $ eval1 t'
  TmIsZero TmZero          -> TmTrue
  TmIsZero (TmSucc TmZero) -> TmFalse
  TmIsZero t'              -> TmIsZero $ eval1 $ if isnumerical t' then t' else NoRuleApplies
  _                        -> NoRuleApplies

isnumerical :: Term -> Bool
isnumerical term = case term of
  TmZero       -> True
  TmSucc term' -> isnumerical term'
  _            -> False
