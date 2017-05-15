module Indexing where

import Types
import Control.Monad.State
import Control.Lens ((^.), (&), (+~))
import Data.List

indexing :: UnIndexedTerm -> IndexedTerm
indexing t = evalState (indexing' t) []

indexing' :: UnIndexedTerm -> State [Name] IndexedTerm
indexing' t = case t of
  TmApp t1 t2           -> TmApp <$> indexing' t1 <*> indexing' t2
  TmLambda nameStr t1   -> do
    modify indexPlusOne
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

indexPlusOne :: [Name] -> [Name]
indexPlusOne = map (\n -> n&index+~1)

type Key = String

findName :: Key -> State [Name] (Maybe Name)
findName key = find (\n -> (n^.name) == key) <$> get

