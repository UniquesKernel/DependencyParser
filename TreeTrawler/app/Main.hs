module Main (main) where

import Text.Megaparsec 
import Gradle.Lexer
import Gradle.Parser
import Gradle.Sanitiser
import Gradle.QueryAndNetworking
import Data.Maybe (catMaybes)
import Data.List (isPrefixOf, stripPrefix)

-- Define the structure of the JSON response
runLex :: [[String]] -> [[TToken]]
runLex = map (concatMap (either (error . show) id . parse lexeme "")) 

runParse :: [[TToken]] -> DependencyTree
runParse tks = DependencyTree "Project" (0) (map (either (error . show) id . parse pDependencyTree "") tks)

cleanInput :: [String] -> String -> [[String]]
cleanInput keywords = filterNoDependecy . filter (not . null) . groupByKeyword keywords . replaceWithKeyword keywords . sanitiseInput keywords . lines

-- Attempts to extract a keyword from a line if it matches the "keyword - ..." format
extractKeyword :: String -> Maybe String
extractKeyword line =
    let trimmed = dropWhile (== ' ') line -- Trim leading spaces if any
    in case span (/= ' ') trimmed of -- Extract the potential keyword before the first space
        (keyword, rest) -> case stripPrefix " - " rest of -- Check if the rest starts with " - "
            Just _ -> Just keyword -- If yes, we've found a keyword
            Nothing -> Nothing -- Otherwise, it's not a valid keyword line

-- Filters and extracts keywords from a list of lines
extractKeywords :: [String] -> [String]
extractKeywords = catMaybes . map extractKeyword

dropLastThree :: [a] -> [a]
dropLastThree xs = take (length xs - 3) xs

main :: IO ()
main = do 
  
  -- read input from file 
  input <- readFile "/home/mokri/TreeTrawler/TreeTrawler/DemoFiles/test.txt" 
  let keywords = dropLastThree . extractKeywords . lines $ input
  let input' = cleanInput keywords input 
  let tks = runLex input' 
  let tree = runParse tks
  print tree


  -- let versionTree = dependencyTreeToVersionTree tree 
  -- filled <- getLatestVersion versionTree
  -- print versionTree

  -- write to csv 
  -- writeVersionTreeToCSV "/home/mokri/TreeTrawler/TreeTrawler/DemoFiles/test.csv" [filled]
