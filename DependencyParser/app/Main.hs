module Main (main) where

import Text.Megaparsec 
import Lexer.Lexer
import Parser.Parser

import Data.List (isPrefixOf)

runLex :: [[String]] -> [[TToken]]
runLex = map (concatMap (either (error . show) id . parse lexeme "")) 

runParse :: [[TToken]] -> DependencyTree
runParse tokens = DependencyTree "Project" (-1) (map (either (error . show) id . parse pDependencyTree "") tokens)

sanitizeInput :: [String] -> [String] -> [String]
sanitizeInput [] _ = []
sanitizeInput (x:xs) keywords
    | any (`isPrefixOf` x) keywords = x:xs
    | otherwise = sanitizeInput xs keywords

-- Function to replace strings that start with a keyword with the keyword itself
replaceWithKeyword :: [String] -> [String] -> [String]
replaceWithKeyword strings keywords = map (replaceIfKeyword keywords) strings

-- Helper function to replace a single string if it starts with a keyword
replaceIfKeyword :: [String] -> String -> String
replaceIfKeyword keywords string = 
    case filter (`isPrefixOf` string) keywords of
        (kw:_) -> kw
        [] -> string

-- Function to group lines by keywords
groupByKeyword :: [String] -> [String] -> [[String]]
groupByKeyword [] _ = []
groupByKeyword strings keywords = groupHelper strings keywords []

-- Recursive helper function for grouping
groupHelper :: [String] -> [String] -> [String] -> [[String]]
groupHelper [] _ currentGroup = [currentGroup | not (null currentGroup)]
groupHelper (x:xs) keywords currentGroup
    | x `elem` keywords = currentGroup : groupHelper xs keywords [x]
    | otherwise = groupHelper xs keywords (currentGroup ++ [x])

keywords :: [String]
keywords = [ "runtimeClassPath"
           , "implementation"
           , "compileClasspath"
           , "runtimeOnly"
           , "testCompileClasspath"
           , "testRuntimeOnly"
           , "testCompileOnly"
           , "compileOnly"
           , "default"
           , "testCompile"
           , "runtimeElements"
           , "annotationProcessor"
           , "apiElements"
           , "archives"
           , "testAnnotationProcessor"
           , "testRuntimeClasspath"
           , "testImplementation"]

filterNoDependecy :: [[String]] -> [[String]] 
filterNoDependecy = map $ filter (/= "No dependencies")

main :: IO ()
main = do 
  -- read input from file 
  input <- readFile "/home/woutan/Experimentation/test.txt"
  
  let input' = filterNoDependecy $ filter (not . null) $ groupByKeyword (replaceWithKeyword (sanitizeInput (lines input) keywords) keywords) keywords
  let tokens = runLex input' 
  let tree = runParse tokens 
  print tree
