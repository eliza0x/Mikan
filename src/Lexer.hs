{-# LANGUAGE OverloadedStrings #-}

module Lexer where

import Types
import Text.ParserCombinators.Parsec

simbolsWithTerm :: Parser Simbol
simbolsWithTerm = spaces *> termParser

termParser :: Parser Simbol
termParser = SmList <$>
  (many (simbols <* spaces) <* eof)

simbols :: Parser Simbol
simbols = 
      listParser
  <|> lambdaParser
  <|> simbol
  <|> space *> simbols

simbol :: Parser Simbol
simbol = SmAtom <$> many1 letter

listParser :: Parser Simbol
listParser = SmList <$> 
  (char '(' *> many (simbols <* spaces) <* char ')')

lambdaParser :: Parser Simbol
lambdaParser = do
  char '\\' *> spaces
  var <- simbol
  spaces <* char '.'
  term <- SmList <$> many (simbols <* spaces)
  return $ SmLambda var term

lexer :: String -> Either ParseError Simbol
lexer = parse simbolsWithTerm "[LEXER]"
