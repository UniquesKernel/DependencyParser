module Main (main) where

import Text.Megaparsec 
import Lexer.Lexer
import Parser.Parser

main :: IO ()
main = do 
  -- read input from file 
  input <- readFile "/home/woutan/Experimentation/test.txt"
  
  let input' = lines input
  let output = concatMap (either (error . show) id . parse lexeme "") input'

  case parse pDependencyTree "" output of 
    Left _ -> error "Failed to parse"
    Right val -> print val

