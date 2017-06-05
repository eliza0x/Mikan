module Main where

import Test.DocTest

main :: IO ()
main = doctest [ "src/Mikan.hs"
               , "src/Core/Core.hs"
               , "src/Core/Lexer.hs"
               , "src/Core/Type.hs"
               ] 
