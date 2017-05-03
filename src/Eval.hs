module Eval(eval) where

import Types

eval :: Term -> Term
eval t = if t == t' then t' else eval t' where
  t' = eval1 t

eval1 :: Term -> Term
eval1 t = case t of
  TmZero                      -> TmZero
  TmTrue                      -> TmTrue
  TmFalse                     -> TmFalse
  TmPred TmZero               -> TmZero
  TmPred (TmPair TmZero t')   -> if isnumerical t' then t' else NoRuleApplies
  TmPred t'                   -> TmPred $ eval1 t'
  TmPair TmZero TmZero        -> TmPair TmZero TmZero
  TmPair TmZero t'            -> TmPair TmZero $ eval1 t'
  TmIsZero TmZero             -> TmTrue
  TmIsZero (TmPair TmZero _)  -> TmFalse
  TmIsZero t'                 -> TmIsZero $ eval1 $ 
                                   if isnumerical t' then t' else NoRuleApplies
  _                           -> NoRuleApplies

isnumerical :: Term -> Bool
isnumerical term = case term of
  TmZero       -> True
  TmPair TmZero term' -> isnumerical term'
  _            -> False
