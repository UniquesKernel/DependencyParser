module Gradle.Parser
( pDependencyTree
, showTreeCSV
, queryDirectDependencies
, DependencyTree (..)
) where

import Gradle.Lexer (TToken)

import Text.Megaparsec 
import Data.Void 


type Parser = Parsec Void [TToken]
data DependencyTree = DependencyTree !String !Int ![DependencyTree] deriving (Eq)

instance Show DependencyTree where
    show = showTree 0
      where
        showTree :: Int -> DependencyTree -> String
        showTree indent (DependencyTree name level children) =
            replicate indent ' ' ++ "- " ++ name ++ " (Level: " ++ show level ++ ")\n" ++
            concatMap (showTree (indent + 2)) children

showTreeCSV :: DependencyTree -> [String]
showTreeCSV = go "" 
  where
    go :: String -> DependencyTree -> [String]
    go prefix (DependencyTree name _ children) =
        let current = if null prefix then name else prefix ++ " > " ++ name
        in current : concatMap (go current) children

pDependencyTree :: Parser DependencyTree 
pDependencyTree = do 
  (lvl, name) <- anySingle 
  next <- optional $ lookAhead anySingle 

  case next of 
    Nothing -> return $ DependencyTree name lvl []
    Just (lvl', _) -> do 
      if lvl' > lvl then
        do 
        children <- manyTill pDependencyTree (lowerLevelOrEnd lvl)
        return $ DependencyTree name lvl children 
      else 
        do
        return $ DependencyTree name lvl []

lowerLevelOrEnd :: Int -> Parser ()
lowerLevelOrEnd lvl = do
  next <- lookAhead $ optional anySingle
  case next of 
    Nothing -> return ()
    Just (lvl', _) -> if lvl' <= lvl then return () else fail "Continue Parsing"


queryDirectDependencies :: DependencyTree -> [DependencyTree]
queryDirectDependencies (DependencyTree name lvl children) = 
  map (\(DependencyTree name lvl _) -> DependencyTree name lvl []) children

