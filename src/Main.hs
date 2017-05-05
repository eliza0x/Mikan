{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (succ, pred)
import System.Environment (getArgs)
import Eval(eval)
import Types
import Text.ParserCombinators.Parsec

term :: Parser Term
term = spaces *> (ifParser <|> trueParser <|> falseParser <|> numericalParser ) <* spaces

ifParser :: Parser Term
ifParser = do
  b <- string "if" *> term
  t <- string "then" *> term
  f <- string "else" *> term
  pure $ TmIf b t f

numericalParser,  numberParser, succParser, predParser, zeroParser :: Parser Term
numericalParser = numberParser <|> succParser <|> predParser <|> zeroParser

zeroParser = string ":mikan:" *> pure TmZero

numberParser = toTerm <$> (char '[' *> elements <* char ']')
  where
    elements = spaces *> many element <* spaces
    element = char '[' *> (term <|> nullParser) <* char ']'
    toTerm []     = TmZero
    toTerm (_:xs) = TmSucc $ toTerm xs
    nullParser = spaces *> pure TmZero

succParser = TmSucc <$> (string "succ" *> term)
predParser = TmPred <$> (string "pred" *> term)
 
trueParser, falseParser :: Parser Term
trueParser    = pure TmTrue  <* string "true"
falseParser   = pure TmFalse <* string "false"

run :: String -> String
run input = case parse term "Mikan" input of
            Left   err -> show err
            Right  val -> show . eval $ val

main :: IO ()
main = putStrLn . run . unwords =<< getArgs
