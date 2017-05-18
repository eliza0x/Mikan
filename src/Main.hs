module Main  where

import Eval(unsafeEval)
import Lexer(lexer)
import Parser(parser)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  source <- prelude <$> case head args of
    "-f" -> readFile . unwords . tail $ args
    _    -> return $ unwords args
  print . unsafeEval . parser $ case lexer source of
      Left err     -> error $ show err
      Right simbol -> simbol

prelude :: String -> String
prelude source = "(\\true.(\\false.(\\if." ++ source ++"))) (\\x.(\\y. x)) (\\x.(\\y. y)) (\\b. (\\t. (\\f. b t f)))"
