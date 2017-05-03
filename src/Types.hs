module Types where

data Term =
    TmIsZero Term
  | TmPair   Term Term
  | TmPred   Term
  | TmSucc   Term
  | TmAdd   Term Term
  | TmSub   Term Term
  | TmMul   Term Term
  | TmHead   Term
  | TmTail   Term
  | TmZero
  | TmTrue
  | TmFalse
  | NoRuleApplies
  deriving Eq

instance Show Term where
    show (TmIsZero x) = "iszero? " ++ show x
    -- show (TmPair x y) = "[" ++ show x ++ " " ++ show y ++ "]"
    show (TmPair x y) = show x ++ " : " ++ show y
    show (TmSucc x)   = "succ " ++ show x
    show (TmPred x)   = "pred " ++ show x
    show (TmAdd x y)   = "add " ++ show x ++ " " ++ show y
    show (TmSub x y)   = "sub " ++ show x ++ " " ++ show y
    show (TmMul x y)   = "mul " ++ show x ++ " " ++ show y
    show (TmHead x)   = "head " ++ show x
    show (TmTail x)   = "tail " ++ show x
    show TmZero       = "[]"
    show TmTrue       = "true"
    show TmFalse      = "false"
    show NoRuleApplies = "<no rule applies>"
