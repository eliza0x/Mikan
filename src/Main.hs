{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (succ, pred)
import Eval(eval)
import Types
import Text.ParserCombinators.Parsec

-- | <term>     ::= "(" <statement> ")" | <numeric> | <boolean> 
-- | <numeric>  ::= pred <list> | <list>
-- | <boolean>  ::= true | false | iszero <numeric>

statement :: Parser Term
statement = spaces *> term <* spaces

term :: Parser Term
term = bracketParser <|> numericalParser <|> booleanParser

bracketParser :: Parser Term
bracketParser = char '(' *> statement <* char ')'

numberParser, numericalParser,  succParser, predParser, addParser, subParser, mulParser :: Parser Term
numericalParser = numberParser <|> succParser <|> predParser 

numberParser =
  listToMikanTerm <$> (char '[' *> spaces *> elements <* spaces <* char ']')
  where
  elements = many $ spaces *> char '[' *> (statement <|> spaces *> pure TmZero) <* char ']' <* spaces
  listToMikanTerm :: [Term] -> Term
  listToMikanTerm [] = TmZero
  listToMikanTerm (_:xs) = TmSucc $ listToMikanTerm xs

succParser = TmSucc <$> (string "succ" *> space *> statement)
predParser = TmPred <$> (string "pred" *> space *> statement)
addParser  = TmAdd <$> (string "add" *> space *> statement) <*> (space *> statement)
mulParser  = TmMul <$> (string "mul" *> space *> statement) <*> (space *> statement)
subParser  = TmSub <$> (string "sub" *> space *> statement) <*> (space *> statement)

booleanParser, isZeroParser, trueParser, falseParser :: Parser Term
booleanParser = trueParser <|> falseParser
isZeroParser = TmIsZero <$> (string "iszero?" *> space *> statement)
trueParser    = pure TmTrue  <* string "true"
falseParser   = pure TmFalse <* string "false"

run :: String -> String
run input = case parse statement "Mikan" input of
            Left   err -> show err
            Right  val -> show . eval $ val

main :: IO ()
main = putStrLn . run =<< readLn

