{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens (makeLenses)
import Control.Monad.Writer(Writer)

type SmVar = String

data Simbol =
    SmAtom String
  | SmLambda Simbol Simbol
  | SmList [Simbol]
  deriving (Show, Eq)

data Term a =
     TmZero
   | TmTrue
   | TmFalse
   | TmIsZero
   | TmPred
   | TmSucc
   | TmIf
   | TmName a
   | TmLambda String (Term a)
   | TmApp (Term a) (Term a)
   | NoRuleApplies String
   deriving Eq

instance Show a => Show (Term a)  where
    show TmIsZero            = "iszero"
    show TmSucc              = "succ"
    show TmPred              = "pred"
    show TmIf                = "if"
    show TmZero              = "zero"
    show TmTrue              = "true"
    show TmFalse             = "false"
    show (TmName n)          = show n
    show (TmLambda str t)      = "(λ" ++ str ++ ". " ++ show t ++ ")"
    show (TmApp t t')        = show t ++ " (" ++ show t' ++ ")"
    show (NoRuleApplies str) = "<" ++ str ++ ">"

data Name = Name {
    _name :: String
  , _index :: Int
  } deriving (Eq)
$(makeLenses ''Name)

instance Show Name  where
  show (Name s _) = s

type UnIndexedTerm = Term String
type IndexedTerm = Term Name

type LoggerM=Writer [IndexedTerm]

