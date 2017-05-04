module Types where

data Term =
    TmIsZero Term
  | TmPred   Term
  | TmSucc   Term
  | TmIf   Term Term Term
  | TmZero
  | TmTrue
  | TmFalse
  | NoRuleApplies
  deriving Eq

instance Show Term where
    show (TmIsZero x) = "iszero? " ++ show x
    -- show (TmPair x y) = "[" ++ show x ++ " " ++ show y ++ "]"
    show (TmSucc x)   = "succ " ++ show x
    show (TmPred x)   = "pred " ++ show x
    show (TmIf x y z)   = "if " ++ show x ++ " then " ++ show y ++ " else " ++ show z
    show TmZero       = "[]"
    show TmTrue       = "true"
    show TmFalse      = "false"
    show NoRuleApplies = "<no rule applies>"
