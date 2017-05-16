module Main where

import System.Environment (getArgs)
import Eval(unsafeEval)
import Lexer(lexer)
import Parser(parser)

main :: IO ()
main = do
  args <- getArgs
  source <- case head args of
    "-f" -> readFile . unwords . tail $ args
    _    -> return $ unwords args
  print . unsafeEval . parser $ case lexer source of
      Left err     -> error $ show err
      Right simbol -> simbol
