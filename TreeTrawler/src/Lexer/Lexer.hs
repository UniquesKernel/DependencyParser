{-# LANGUAGE CPP #-}
module Lexer.Lexer 
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

{- | 
Processes a stream of strings and returns a stream of TTokens 
the precedence of "+---", "\---", "|" or "     " (5 empty spaces)
constitute one level of indentation indicating used to indicate if 
a dependency in a gradle dependency tree is a transitive, sibling or 
none related dependency to the dependecy that came before.
--
__Examples__ 

>>> parse lexeme "" ["runtimeClasspath", "+--- "repo:name:version1", "|   +--- repo2:name2:version2", "testImplementation"] 
--  Right [(0, "runtimeClasspath"), (1, "repo:name:version1"), (2, "repo2:name2:version"), (0, "testImplementation")]
--
-}
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
