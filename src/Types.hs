module Types where

data Simbol =
    SmAtom String
  | SmList [Simbol]
  deriving (Show, Eq)

type TmValue = String

data Term =
     TmZero
   | TmTrue
   | TmFalse
   | TmIsZero
   | TmPred
   | TmSucc
   | TmIf
   | TmLambda  TmValue Term
   | TmApp Term Term
   | NoRuleApplies String
   deriving Eq

instance Show Term where
    show TmIsZero            = "iszero"
    show TmSucc              = "succ"
    show TmPred              = "pred"
    show TmIf                = "if"
    show TmZero              = "zero"
    show TmTrue              = "true"
    show TmFalse             = "false"
    show (TmLambda v t)      = "(λ" ++ v ++ ". " ++ show t ++ ")"
    show (TmApp t t')      = show t ++ " " ++ show t'
    show (NoRuleApplies str) = "<" ++ str ++ ">"
