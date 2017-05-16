module Indexing where

import Types
import Control.Monad.State
import Control.Lens ((^.), (&), (+~), (-~))
import Data.List

indexing :: UnIndexedTerm -> IndexedTerm
indexing t = evalState (indexing' t) []

indexing' :: UnIndexedTerm -> State [Name] IndexedTerm
indexing' t = case t of
  TmApp t1 t2           -> TmApp <$> indexing' t1 <*> indexing' t2
  TmLambda nameStr t1   -> do
    modify incrementIndex
    modify (\xs -> Name nameStr 0 : xs)
    TmLambda nameStr <$> indexing' t1
  TmName nameStr        -> do
    nameMaybe <- findName nameStr
    return $ case nameMaybe of
      Nothing   -> NoRuleApplies ("undefined value:" ++ nameStr)
      Just n    -> TmName . Name nameStr $ (n^.index)
  TmZero                -> return TmZero           
  TmTrue                -> return TmTrue           
  TmFalse               -> return TmFalse          
  TmIsZero              -> return TmIsZero         
  TmPred                -> return TmPred           
  TmSucc                -> return TmSucc           
  TmIf                  -> return TmIf             
  NoRuleApplies str     -> return $ NoRuleApplies str

incrementIndex :: [Name] -> [Name]
incrementIndex = map (\n -> n&index+~1)

decrementIndex :: [Name] -> [Name]
decrementIndex = map (\n -> n&index-~1)

type Key = String

findName :: Key -> State [Name] (Maybe Name)
findName key = find (\n -> (n^.name) == key) <$> get

betaReduction :: IndexedTerm -> IndexedTerm -> IndexedTerm
betaReduction = betaReduction' 0

betaReduction' :: Int         -- index
              -> IndexedTerm -- source
              -> IndexedTerm -- source内のindexと一致したものをこの項で置換
              -> IndexedTerm -- output
betaReduction' i t1 var = case t1 of
  TmApp t11 t12     -> TmApp (betaReduction' i t11 var) (betaReduction' i t12 var)
  TmLambda n t11    -> TmLambda n $ betaReduction' (i+1) t11 var
  TmName n          -> if i == n^.index then var else TmName n
  TmZero            -> TmZero           
  TmTrue            -> TmTrue           
  TmFalse           -> TmFalse          
  TmIsZero          -> TmIsZero         
  TmPred            -> TmPred           
  TmSucc            -> TmSucc           
  TmIf              -> TmIf             
  NoRuleApplies str -> NoRuleApplies str



