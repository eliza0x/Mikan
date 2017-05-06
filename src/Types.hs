module Types where

data Simbol =
    SmAtom String
  | SmList [Simbol]
  deriving (Show, Eq)

data Term =
    TmZero
  | TmTrue
  | TmFalse
  | TmIsZero Term
  | TmPred   Term
  | TmSucc   Term
  | TmIf   Term Term Term
  | NoRuleApplies String
  deriving Eq

instance Show Term where
    show (TmIsZero x) = "iszero? " ++ show x
    show (TmSucc x)   = "succ " ++ show x
    show (TmPred x)   = "pred " ++ show x
    show (TmIf x y z)   = "if " ++ show x ++ " then " ++ show y ++ " else " ++ show z
    show TmZero       = "[]"
    show TmTrue       = "true"
    show TmFalse      = "false"
    show (NoRuleApplies str) = "[" ++ str ++ "]"
