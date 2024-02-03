module Main (main) where

import Text.Megaparsec 
import Gradle.Lexer
import Gradle.Parser
import Gradle.Sanitiser
import Gradle.QueryAndNetworking

-- Define the structure of the JSON response
runLex :: [[String]] -> [[TToken]]
runLex = map (concatMap (either (error . show) id . parse lexeme "")) 

runParse :: [[TToken]] -> DependencyTree
runParse tks = DependencyTree "Project" (-1) (map (either (error . show) id . parse pDependencyTree "") tks)

cleanInput :: String -> [[String]]
cleanInput = filterNoDependecy . filter (not . null) . groupByKeyword keywords . replaceWithKeyword keywords . sanitiseInput keywords . lines

main :: IO ()
main = do 
  
  -- read input from file 
  input <- readFile "/home/woutan/Experimentation/test.txt"
  
  let input' = cleanInput input 
  let tks = runLex input' 
  let tree = runParse tks
  -- print tree

  let versionTree = dependencyTreeToVersionTree tree 
  filled <- getLatestVersion versionTree

  -- write to csv 
  writeVersionTreeToCSV "/home/woutan/Experimentation/test.csv" [filled]
