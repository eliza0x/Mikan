module Main where

data Term =
    TmIsZero Term
  | TmSucc   Term
  | TmPred   Term
  | TmZero
  | TmTrue
  | TmFalse
  | NoRuleApplies
  deriving Show

eval1 :: Term -> Term
eval1 term = case term of
  TmZero                -> TmZero
  TmTrue                -> TmTrue
  TmFalse               -> TmFalse
  TmPred TmZero         -> TmZero
  TmPred (TmSucc term') -> if isnumerical term' then term' else NoRuleApplies
  TmSucc TmZero         -> TmSucc TmZero
  TmSucc term'          -> TmSucc $ eval1 term'
  TmIsZero TmZero       -> TmTrue
  TmIsZero (TmSucc _)   -> TmFalse
  TmIsZero term'        -> TmIsZero $ eval1 $ if isnumerical term' then term' else NoRuleApplies  
  _                     -> NoRuleApplies

isnumerical :: Term -> Bool
isnumerical term = case term of
  TmZero       -> True
  TmSucc term' -> isnumerical term'
  _            -> False

main :: IO ()
main =
  putStrLn "hello world"

