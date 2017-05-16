module Main where

import System.Environment (getArgs)
import Eval(unsafeEval)
import Lexer(lexer)
import Parser(parser)

main :: IO ()
main = do
  source <- readFile . unwords =<< getArgs
  print . unsafeEval . parser $ case lexer source of
      Left err     -> error $ show err
      Right simbol -> simbol
