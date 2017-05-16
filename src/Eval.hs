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
    TmTrue                                   -> return TmTrue
    TmFalse                                  -> return TmFalse
    TmApp(TmApp(TmApp TmIf TmTrue)  t') _    -> return t'
    TmApp(TmApp(TmApp TmIf TmFalse) _)  t'   -> return t'
    TmApp(TmApp(TmApp TmIf t')iftrue)iffalse -> evalIfEval1 t' iftrue iffalse
    TmApp TmSucc t'                          -> evalTmSucc t'
    TmApp TmPred TmZero                      -> return TmZero
    TmApp TmPred (TmApp TmSucc t')           -> evalPredSucc t'
    TmApp TmPred t'                          -> TmApp TmPred <$> eval1 t'
    TmApp TmIsZero TmZero                    -> return TmTrue
    TmApp TmIsZero (TmApp TmSucc TmZero)     -> return TmFalse
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

evalIfEval1 :: IndexedTerm -> IndexedTerm -> IndexedTerm -> StateT [Name] LoggerM IndexedTerm
evalIfEval1 term iftrue iffalse = (\t'-> TmApp (TmApp (TmApp TmIf t') iftrue) iffalse) 
                                <$> eval1 term

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
  TmApp (TmApp (TmApp TmIf _) t1) t2 -> isnumericval t1 && isnumericval t2
  TmApp (TmLambda _ term') _         -> isnumericval term'
  _                                  -> False

