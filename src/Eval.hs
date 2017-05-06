module Eval(eval, unsafeEval) where

import Types
import Control.Monad.Writer.Lazy (Writer, tell, runWriter)
import System.IO.Unsafe

eval :: Term -> Term
eval = fst . runWriter . eval'

unsafeEval :: Term -> Term
unsafeEval t = unsafePerformIO $ do
         let (evaluatedTerm, logHead:logTail) = runWriter . eval' $ t
         print logHead
         mapM_ (\term -> putStrLn $ "-> " ++ show term) logTail
         putStrLn "---"
         return evaluatedTerm

eval' :: Term -> Writer [Term] Term
eval' t = do
  t' <- eval1 t
  if t == t' then return t'
             else eval' t'

eval1 :: Term -> Writer [Term] Term
eval1 t = tell [t] >> (case t of
  TmZero                   -> return TmZero
  TmTrue                   -> return TmTrue
  TmFalse                  -> return TmFalse
  TmIf TmTrue  t' _        -> return t'
  TmIf TmFalse _  t'       -> return t'
  TmIf t' iftrue  iffalse  -> (\t'' -> TmIf t'' iftrue iffalse) <$> eval1 t'
  TmSucc t'                -> TmSucc <$> if isnumericval t' 
                                  then eval1 t' 
                                  else return (NoRuleApplies "Missing evaluate: succ(<not numeric>)")
  TmPred TmZero            -> return TmZero
  TmPred (TmSucc t')       -> return $ if isnumericval t' 
                                then t'
                                else NoRuleApplies "Missing evaluate: pred(succ(<not numeric>))"
  TmPred t'                -> TmPred <$> eval1 t'
  TmIsZero TmZero          -> return TmTrue
  TmIsZero (TmSucc TmZero) -> return TmFalse
  TmIsZero t'              -> TmIsZero <$> eval1 (if isnumericval t' 
                                 then t' 
                                 else NoRuleApplies "Missing evaluate: TmIsZero(not numeric)")
  _                        -> return $ NoRuleApplies "Missing evaluate: nothing match evaluate pattern"
  )

isnumericval :: Term -> Bool
isnumericval term = case term of
  TmZero       -> True
  TmSucc term' -> isnumericval term'
  TmPred term' -> isnumericval term'
  _            -> False
