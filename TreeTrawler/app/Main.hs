{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Main (main) where

import Text.Megaparsec 
import Lexer.Lexer
import Parser.Parser

import Data.List (isPrefixOf)

import Network.HTTP.Simple
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as L8

-- Define the data structure based on the JSON response
data MavenResponse = MavenResponse
  { response :: ResponseBody
  } deriving (Show, Generic)

data ResponseBody = ResponseBody
  { docs :: [Doc]
  } deriving (Show, Generic)

data Doc = Doc
  { v :: String -- Version
  } deriving (Show, Generic)

instance FromJSON MavenResponse
instance FromJSON ResponseBody
instance FromJSON Doc where
    parseJSON = withObject "Doc" $ \v -> Doc
        <$> v .: "v"

-- Define the structure of the JSON response
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
 -- print tree

  let uris = map mapToUri $ map extractGAV $ extractNames $ extractLevel2 tree 3

  requests' <- mapM parseRequest $ uris
  let requests = map (setRequestHeader "User-Agent" ["MyCustomUserAgent/1.0"]) requests'

  print requests'
  responses <- mapM httpLBS requests

  let jsonResponses = map (eitherDecode . getResponseBody) responses :: [Either String MavenResponse]
  let versions = map (either (error . show) id . fmap extractLatestVersion) jsonResponses
  print versions
--  request' <- parseRequest "https://search.maven.org/solrsearch/select?q=g:com.google.inject+AND+a:guice&core=gav&rows=20&wt=json"
 -- let request = setRequestHeader "User-Agent" ["MyCustomUserAgent/1.0"] request'
--  response <- httpLBS request

--  let jsonResponse = eitherDecode $ getResponseBody response :: Either String MavenResponse
--  case jsonResponse of
--    Left err -> putStrLn err
--    Right res -> putStrLn $ extractLatestVersion res

extractLatestVersion :: MavenResponse -> String
extractLatestVersion = v . head . docs . response

-- Function to extract the n'th level of the dependency tree 
-- leave children empty to not extract children 

-- map all DependencyTree's to themselves with empty children 
extractLevel2 :: DependencyTree -> Int -> [DependencyTree] 
extractLevel2 (DependencyTree name level children) n 
  | level == n = [DependencyTree name level []] 
  | otherwise = concatMap (extractLevel2' n) children
    where 
      extractLevel2' :: Int -> DependencyTree -> [DependencyTree] 
      extractLevel2' n (DependencyTree name level children) 
        | level == n = [DependencyTree name level []] 
        | otherwise = concatMap (extractLevel2' n) children

-- map all DependencyTree's to their names
extractNames :: [DependencyTree] -> [String] 
extractNames [] = []
extractNames ((DependencyTree name lvl children):xs)= name : extractNames xs

-- extract groupId and artifactId from a string 
extractGAV :: String -> (String, String, String) 
extractGAV s = splitOn ':' s 
  where 
    splitOn :: Char -> String -> (String, String, String) 
    splitOn c s = (g, a, v) 
      where 
        g = takeWhile (/= c) s 
        s' = drop (length g + 1) s 
        a = takeWhile (/= c) s' 
        v = drop (length a + 1) s'

-- map to uri 
mapToUri :: (String, String, String) -> String 
mapToUri (g, a, v) = "https://search.maven.org/solrsearch/select?q=g:" ++ g ++ "+AND+a:" ++ a ++ "&core=gav&rows=20&wt=json"
