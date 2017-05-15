module Main where

import System.Environment (getArgs)
import Eval(unsafeEval)
import Lexer(lexer)
import Indexing(indexing)
import Parser(parser)

main :: IO ()
main = do
  source <- unwords <$> getArgs
  print . unsafeEval . indexing . parser $ case lexer source of
      Left err     -> error $ show err
      Right simbol -> simbol
