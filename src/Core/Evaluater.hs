{-# LANGUAGE FlexibleContexts #-}

module Core.Evaluater
  ( evaluater
  ) where

import Control.Monad.IO.Class
import Control.Monad.State.Class
import Control.Monad.Catch

import Core.Type

-- |
-- 木構造に整理されたプログラムを評価する
-- 
evaluater :: (MonadThrow m, MonadIO m, MonadState NameSpace m)
          => IndexedTerm -> m ()
evaluater _ = return ()

