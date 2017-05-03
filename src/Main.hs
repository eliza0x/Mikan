module Main where

data Term =
    TmIsZero Term
  | TmSucc   Term
  | TmPred   Term
  | TmZero
  | TmTrue
  | TmFalse
  | NoRuleApplies
  deriving (Show, Eq)

eval :: Term -> Term
eval t = if t == t' then t' else eval t' where
  t' = eval1 t

eval1 :: Term -> Term
eval1 t = case t of
  TmZero              -> TmZero
  TmTrue              -> TmTrue
  TmFalse             -> TmFalse
  TmPred TmZero       -> TmZero
  TmPred (TmSucc t')  -> if isnumerical t' then t' else NoRuleApplies
  TmPred t'           -> TmPred $ eval1 t'
  TmSucc TmZero       -> TmSucc TmZero
  TmSucc t'           -> TmSucc $ eval1 t'
  TmIsZero TmZero     -> TmTrue
  TmIsZero (TmSucc _) -> TmFalse
  TmIsZero t'         -> TmIsZero $ eval1 $ if isnumerical t' then t' else NoRuleApplies
  _                   -> NoRuleApplies

isnumerical :: Term -> Bool
isnumerical term = case term of
  TmZero       -> True
  TmSucc term' -> isnumerical term'
  _            -> False

main :: IO ()
main =
  putStrLn "hello world"

