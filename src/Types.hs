module Types where

data Term =
    TmIsZero Term
  | TmSucc   Term
  | TmPred   Term
  | TmZero
  | TmTrue
  | TmFalse
  | NoRuleApplies
  deriving (Show, Eq)
