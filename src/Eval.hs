module Eval(eval, unsafeEval) where

import Types
import Indexing
import Control.Monad.Writer.Lazy (tell, runWriter)
import Control.Monad.State
import System.IO.Unsafe

eval :: UnIndexedTerm -> IndexedTerm
eval = fst . runEval . eval' . indexing

unsafeEval :: UnIndexedTerm -> IndexedTerm
unsafeEval t = unsafePerformIO $ do
         let (evaluatedTerm, logHead:logTail) = runEval . eval' . indexing $ t
         print logHead
         mapM_ (\term -> putStrLn $ "-> " ++ show term) logTail
         putStrLn "---"
         return evaluatedTerm

runEval :: StateT [Name] LoggerM IndexedTerm -> (IndexedTerm, [IndexedTerm])
runEval = runWriter . (`evalStateT` [])

eval' :: IndexedTerm -> StateT [Name] LoggerM IndexedTerm
eval' t = do
  t' <- eval1 t
  if t == t' then return t'
             else eval' t'

eval1 :: IndexedTerm -> StateT [Name] LoggerM IndexedTerm
eval1 t = do
  lift (tell [t])
  case t of
    TmZero                                   -> return TmZero
    TmApp TmSucc t'                          -> evalTmSucc t'
    TmApp TmPred TmZero                      -> return TmZero
    TmApp TmPred (TmApp TmSucc t')           -> evalPredSucc t'
    TmApp TmPred t'                          -> TmApp TmPred <$> eval1 t'
    TmApp TmIsZero TmZero                    -> return $ TmLambda "x" (TmLambda "y" (TmName (Name "x" 1)))
    TmApp TmIsZero (TmApp TmSucc TmZero)     -> return $ TmLambda "x" (TmLambda "y" (TmName (Name "y" 0)))
    TmApp TmIsZero t'                        -> evalIsZero t'
    TmApp (TmLambda _ t1) t2                 -> do modify decrementIndex
                                                   return $ betaReduction t1 t2
    TmApp t1 t2                              -> TmApp <$> eval1 t1 <*> eval1 t2
    NoRuleApplies str                        -> return $ NoRuleApplies str
    x                                        -> return x

evalTmSucc, evalIsZero, evalPredSucc :: IndexedTerm -> StateT [Name] LoggerM IndexedTerm
evalTmSucc t' = TmApp TmSucc <$> if isnumericval t'
                                 then eval1 t'
                                 else return (NoRuleApplies succNotNumeric)
evalIsZero t' = TmApp TmIsZero <$> eval1 (if isnumericval t'
                                          then t'
                                          else NoRuleApplies iszeroNotNumeric)
evalPredSucc t' = return $ if isnumericval t'
                         then t'
                         else NoRuleApplies predNotNumeric 

iszeroNotNumeric, succNotNumeric, predNotNumeric :: String
iszeroNotNumeric = "Missing evaluate: TmIsZero(not numeric)"
succNotNumeric   = "Missing evaluate: succ(<not numeric>)"
predNotNumeric   = "Missing evaluate: pred(succ(<not numeric>))"
-- nothingMatch    = "Missing evaluate: nothing match evaluate pattern"

isnumericval :: IndexedTerm -> Bool
isnumericval term = case term of
  TmZero               -> True
  TmApp TmSucc term'                 -> isnumericval term'
  TmApp TmPred term'                 -> isnumericval term'
  TmApp (TmLambda _ term') _         -> isnumericval term'
  _                                  -> False

