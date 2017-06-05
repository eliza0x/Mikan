{-# LANGUAGE FlexibleContexts #-}

module Core.Parser
  ( parser
  )where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.State (MonadState)
import Core.Type (Token(..), IndexedTerm, NameSpace)

-- |
-- 最小単位に分解された文字を
-- 
-- 1. 木に変換する
-- 3. 名前から解放して、de brujin indexingする
-- 2. 名前空間を生成する
--
-- >>> parser $ L [A "if", A "true", A "zero", A "false"]
-- (if true zero false)
--
parser :: (MonadThrow m, MonadState NameSpace m)
       => Token -> m IndexedTerm
parser _ = undefined

