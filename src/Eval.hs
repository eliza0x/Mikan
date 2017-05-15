module Eval(eval, unsafeEval) where

import Types
import Control.Monad.Writer.Lazy (Writer, tell, runWriter)
import System.IO.Unsafe

eval :: IndexedTerm -> IndexedTerm
eval = fst . runWriter . eval'

unsafeEval :: IndexedTerm -> IndexedTerm
unsafeEval t = unsafePerformIO $ do
         let (evaluatedIndexedTerm, logHead:logTail) = runWriter . eval' $ t
         print logHead
         mapM_ (\term -> putStrLn $ "-> " ++ show term) logTail
         putStrLn "---"
         return evaluatedIndexedTerm

eval' :: IndexedTerm -> Writer [IndexedTerm] IndexedTerm
eval' t = do
  t' <- eval1 t
  if t == t' then return t'
             else eval' t'

eval1 :: IndexedTerm -> Writer [IndexedTerm] IndexedTerm
eval1 t = tell [t] >> (case t of
  TmZero                                    -> return TmZero
  TmTrue                                    -> return TmTrue
  TmFalse                                   -> return TmFalse
  TmApp (TmApp (TmApp TmIf TmTrue)  t') _   -> return t'
  TmApp (TmApp (TmApp TmIf TmFalse) _)  t'  -> return t'
  TmApp (TmApp(TmApp TmIf t')iftrue)iffalse -> evalIfEval1 t' iftrue iffalse
  TmApp TmSucc t'                           -> evalTmSucc t'
  TmApp TmPred TmZero                       -> return TmZero
  TmApp TmPred (TmApp TmSucc t')            -> evalPredSucc t'
  TmApp TmPred t'                           -> TmApp TmPred <$> eval1 t'
  TmApp TmIsZero TmZero                     -> return TmTrue
  TmApp TmIsZero (TmApp TmSucc TmZero)      -> return TmFalse
  TmApp TmIsZero t'                         -> evalIsZero t'
  NoRuleApplies str                         -> return $ NoRuleApplies str
  _                                         -> return $ NoRuleApplies nothingMatch
  )

evalTmSucc, evalIsZero, evalPredSucc :: IndexedTerm -> Writer [IndexedTerm] IndexedTerm
evalTmSucc t' = TmApp TmSucc <$> if isnumericval t'
                                 then eval1 t'
                                 else return (NoRuleApplies succNotNumeric)
evalIsZero t' = TmApp TmIsZero <$> eval1 (if isnumericval t'
                                          then t'
                                          else NoRuleApplies iszeroNotNumeric)
evalPredSucc t' = return $ if isnumericval t'
                         then t'
                         else NoRuleApplies predNotNumeric 

evalIfEval1 :: IndexedTerm -> IndexedTerm -> IndexedTerm -> Writer [IndexedTerm] IndexedTerm
evalIfEval1 term iftrue iffalse = (\t'-> TmApp (TmApp (TmApp TmIf t') iftrue) iffalse) 
                                <$> eval1 term

iszeroNotNumeric, succNotNumeric, predNotNumeric, nothingMatch :: String
iszeroNotNumeric = "Missing evaluate: TmIsZero(not numeric)"
succNotNumeric   = "Missing evaluate: succ(<not numeric>)"
predNotNumeric   = "Missing evaluate: pred(succ(<not numeric>))"
nothingMatch    = "Missing evaluate: nothing match evaluate pattern"

isnumericval :: IndexedTerm -> Bool
isnumericval term = case term of
  TmZero               -> True
  TmApp TmSucc term' -> isnumericval term'
  TmApp TmPred term' -> isnumericval term'
  _                    -> False
