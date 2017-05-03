{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (succ, pred)
import Eval(eval)
import Types
import Text.ParserCombinators.Parsec
-- import Text.Parsec.Prim ((<|>))

-- | <term>     ::= <numeric> | <boolean>
-- | <list>     ::= [<term> <term>]
-- | <numeric>  ::= pred <list> | <list>
-- | <boolean>  ::= true | false | iszero <numeric>

statement :: Parser Term
statement = spaces *> term <* spaces

term :: Parser Term
term = bracketParser <|> succParser <|> predParser <|> booleanParser

numericalParser,  succParser, predParser :: Parser Term
numericalParser = succParser <|> predParser 

bracketParser :: Parser Term
bracketParser =
  listToMikanTerm <$> (char '[' *> spaces *> elements <* spaces <* char ']')
  where
  elements = many $
    spaces *> char '[' *> (statement <|> (spaces *> pure TmZero)) <* char ']' <* spaces
  listToMikanTerm :: [Term] -> Term
  listToMikanTerm []     = TmZero
  listToMikanTerm xs = foldr TmPair TmZero xs

succParser = TmPair TmZero <$> (string "succ" *> many1 space *> statement)
predParser = TmPred <$> (string "pred" *> many1 space *> statement)

booleanParser, isZeroParser, trueParser, falseParser :: Parser Term
booleanParser = trueParser <|> falseParser
isZeroParser = TmIsZero <$> (string "iszero?" *> many1 space *> statement)
trueParser    = pure TmTrue  <* string "true"
falseParser   = pure TmFalse <* string "false"

run :: String -> String
run input = case parse statement "Mikan" input of
            Left   err -> show err
            Right  val -> show . eval $ val

main :: IO ()
main = do
  putStrLn $ run "succ [succ [[] [] []]]"
  putStrLn $ run "pred [[] [] []]"
  putStrLn $ run "pred succ [[] [] []]"

