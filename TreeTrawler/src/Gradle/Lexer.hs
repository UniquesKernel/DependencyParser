{-# LANGUAGE CPP #-}
module Gradle.Lexer 
( lexeme
, TToken 
, Lexer
#ifdef TEST 
, pSpace
, pPipe
, pPlus
, pBackslash
, pIndent
, pDependency
, pSpaceIndent
#endif
) where

import Text.Megaparsec 
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L 
import Data.Void

type TToken = (Int, String)
type Lexer = Parsec Void String

lexeme :: Lexer [TToken]
lexeme = do 
  indent <- pIndent 
  dependency <- pDependency 
  eof
  return [(indent, dependency)]

pDependency :: Lexer String 
pDependency = do 
  dependency <- many (noneOf "\n")
  pSpace
  return dependency

pIndent :: Lexer Int
pIndent = do
  leadingIndent <- sum <$> many pSpaceIndent
  indent <- sum <$> many (pPipe <|> pPlus <|> pBackslash)
 
  return $ indent + leadingIndent

pSpaceIndent :: Lexer Int
pSpaceIndent = do
  _ <- string "     " -- deals with leading tabs and spaces with correct indentation level
  return 1

pSpace :: Lexer ()
pSpace = do 
  L.space space1 empty empty
  return ()

pPipe :: Lexer Int
pPipe = do 
  _ <- char '|'
  pSpace
  return 1

pPlus :: Lexer Int 
pPlus = do 
  _ <- string "+---"
  pSpace
  return 1

pBackslash :: Lexer Int 
pBackslash = do 
  _ <- string "\\---"
  pSpace
  return 1
