module Core.Type
  ( Token(..)
  , UnIndexedTerm
  , IndexedTerm
  , Term(..)
  , NameSpace
  ) where

import Data.Map.Strict (Map)
import Prelude hiding (True, False)

data Token = A String  -- Atom
           | L [Token] -- List
           deriving (Show, Eq)

data Term a =
    Succ (Term a)
  | Pred (Term a)
  | Zero
  | IsZero (Term a)
  | If (Term a) (Term a) (Term a)
  | True
  | False
  | Var a
  | Lambda String (Term a)
  deriving Eq

type UnIndexedTerm = Term String
type IndexedTerm   = Term Int

type NameSpace = Map Int (Term Int)

instance Show a => Show (Term a) where
  show (IsZero t)     = "iszero " ++ show t
  show (Succ t)       = "succ "   ++ show t
  show (Pred t)       = "pred "   ++ show t
  show (If t t' t'')  = "if "     ++ unwords (map show [t, t', t''])
  show True           = "true"
  show False          = "false"
  show Zero           = "zero"
  show (Var n)        = show n
  show (Lambda str t) = "(λ" ++ str ++ ". " ++ show t ++ ")"

