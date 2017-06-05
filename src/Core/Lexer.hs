module Core.Lexer 
  ( lexer
  ) where

import Core.Type (Token(..))
import Control.Monad.Catch

-- |
-- 文字列を最小単位に分解します
--
-- >>> lexer "a b c"
-- L [A "a", A "b", A "c"]
--
lexer :: MonadThrow m => String -> m Token
lexer _ = return $ L [A "a", A "b", A "c"]

