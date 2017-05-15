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
  TmZero                                             -> return TmZero
  TmTrue                                             -> return TmTrue
  TmFalse                                            -> return TmFalse
  TmApp (TmApp (TmApp TmIf TmTrue)  t') _      -> return t'
  TmApp (TmApp (TmApp TmIf TmFalse) _)  t'     -> return t'
  TmApp (TmApp (TmApp TmIf t') iftrue) iffalse -> (\t'' -> TmApp (TmApp (TmApp TmIf t'') iftrue) iffalse) <$> eval1 t'
  TmApp TmSucc t'                                  -> TmApp TmSucc <$> if isnumericval t'
                                                                           then eval1 t'
                                                                           else return (NoRuleApplies succNotNumeric)
  TmApp TmPred TmZero                              -> return TmZero
  TmApp TmPred (TmApp TmSucc t')                 -> return $ if isnumericval t'
                                                            then t'
                                                            else NoRuleApplies predNotNumeric 
  TmApp TmPred t'                                  -> TmApp TmPred <$> eval1 t'
  TmApp TmIsZero TmZero                            -> return TmTrue
  TmApp TmIsZero (TmApp TmSucc TmZero)           -> return TmFalse
  TmApp TmIsZero t'                                -> TmApp TmIsZero <$> eval1 (if isnumericval t'
                                                          then t'
                                                          else NoRuleApplies iszeroNotNumeric)
  NoRuleApplies str                                  -> return $ NoRuleApplies str
  _                        -> return $ NoRuleApplies nothingMatch
  )
  where
  iszeroNotNumeric = "Missing evaluate: TmIsZero(not numeric)"
  succNotNumeric   = "Missing evaluate: succ(<not numeric>)"
  predNotNumeric   = "Missing evaluate: pred(succ(<not numeric>))"
  nothingMatch    = "Missing evaluate: nothing match evaluate pattern"

isnumericval :: Term -> Bool
isnumericval term = case term of
  TmZero               -> True
  TmApp TmSucc term' -> isnumericval term'
  TmApp TmPred term' -> isnumericval term'
  _                    -> False
