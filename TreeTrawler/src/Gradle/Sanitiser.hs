{-# LANGUAGE CPP #-}
module Gradle.Sanitiser 
( replaceWithKeyword
, groupByKeyword
, sanitiseInput
, filterNoDependecy
, keywords
#ifdef TEST 
, replaceLineWithKeyword
#endif
) where

import Data.List (isPrefixOf)

keywords :: [String] 
keywords = [ "annotationProcessor"
           , "archives" 
           , "apiElements"
           , "compileClasspath"
           , "compileOnly" 
           , "default" 
           , "implementation" 
           , "runtimeElements"
           , "runtimeClasspath"
           , "testCompileClasspath" 
           , "testRuntimeClasspath" 
           , "testImplementation" 
           , "testRuntimeOnly" 
           , "testAnnotationProcessor"
           , "testCompile"
           ]

replaceLineWithKeyword :: [String] -> String -> String
replaceLineWithKeyword kws line = 
  case words line of 
    [] -> ""
    (x:_) -> if x `elem` kws then x else line   

replaceWithKeyword :: [String] -> [String] -> [String]
replaceWithKeyword kws = 
  map (replaceLineWithKeyword kws)

groupByKeyword :: [String] -> [String] -> [[String]]
groupByKeyword _ [] = [] 
groupByKeyword kws lines' = filter (not . null) $ groupKeywords kws lines' []
  where 
    groupKeywords :: [String] -> [String] -> [String] -> [[String]]
    groupKeywords _ [] acc = [acc | not (null acc)]
    groupKeywords kws' (y:ys) acc 
      | y `elem` kws' = acc : groupKeywords kws' ys [y]
      | otherwise = groupKeywords kws' ys (acc ++ [y])

sanitiseInput :: [String] -> [String] -> [String]
sanitiseInput _ [] = []
sanitiseInput kws (x:xs) 
  | any (`isPrefixOf` x) kws = x:xs 
  | otherwise = sanitiseInput kws xs


-- | currently Untests as the function may change soon
filterNoDependecy :: [[String]] -> [[String]] 
filterNoDependecy = map $ filter (/= "No dependencies")
