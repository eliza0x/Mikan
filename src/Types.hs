module Types where

data Term =
    TmIsZero Term
  | TmPair   Term Term
  | TmPred   Term
  | TmZero
  | TmTrue
  | TmFalse
  | NoRuleApplies
  deriving Eq

instance Show Term where
    show (TmIsZero x) = "iszero? " ++ show x
    -- show (TmPair x y) = "[" ++ show x ++ " " ++ show y ++ "]"
    show (TmPair x y) = show x ++ " : " ++ show y
    show (TmPred x)   = "pred " ++ show x
    show TmZero       = "[]"
    show TmTrue       = "true"
    show TmFalse      = "false"
    show NoRuleApplies = "<no rule applies>"
