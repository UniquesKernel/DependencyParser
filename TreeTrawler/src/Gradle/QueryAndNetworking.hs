{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Gradle.QueryAndNetworking 
( MavenResponse(..)
, ResponseBody(..)
, Doc(..)
, toNameTree
, dependencyTreeToNameTree
, NameTree(..) 
, VersionTree(..) 
, dependencyTreeToVersionTree 
, getLatestVersion 
, extractGAV 
, extractLatestVersion 
, writeVersionTreeToCSV 
, flattenVersionTree
, VersionRecord(..)
) where

import Gradle.Parser (DependencyTree(..))
import GHC.Generics
import Data.Aeson
import Network.HTTP.Simple
import Data.Csv hiding ((.:))
import qualified Data.Vector as V 
import qualified Data.ByteString.Lazy as BL

newtype MavenResponse = MavenResponse
  { response :: ResponseBody
  } deriving (Show, Generic)

newtype ResponseBody = ResponseBody
  { docs :: [Doc]
  } deriving (Show, Generic)

newtype Doc = Doc
  { version :: String -- Version
  } deriving (Show, Generic)

data NameTree = NameTree !String !Int ![NameTree] 

data VersionTree = VersionTree !(String, String, String) !String !Int ![VersionTree]

instance Show NameTree where
  show = showNameTree 0 
    where 
      showNameTree :: Int -> NameTree -> String
      showNameTree lvl (NameTree name lvl' subDeps) = 
        replicate lvl ' ' ++ name ++ " (Level: " ++ show lvl' ++ ")\n" ++ concatMap (showNameTree (lvl + 1)) subDeps

instance Show VersionTree where
  show = showVersionTree 0
    where 
      showVersionTree :: Int -> VersionTree -> String
      showVersionTree lvl (VersionTree (groupId, artifactId, currentVersion) latestVersion _ transitiveDependencies) = 
        replicate lvl ' ' ++ groupId ++ ":" ++ artifactId ++ ":" ++ currentVersion ++ " (Latest version: " ++ latestVersion ++ ")\n" ++ concatMap (showVersionTree (lvl + 1)) transitiveDependencies

instance FromJSON MavenResponse
instance FromJSON ResponseBody
instance FromJSON Doc where
    parseJSON = withObject "Doc" $ \vers -> Doc <$> vers .: "v"

data VersionRecord = VersionRecord {
  groupId :: String,
  artifactId :: String,
  currentVersion :: String,
  latestVersion :: String,
  indentationLevel :: Int
} deriving (Generic, Show)

instance ToRecord VersionRecord
instance ToNamedRecord VersionRecord
instance DefaultOrdered VersionRecord

extractGAV :: String -> (String, String, String)
extractGAV = splitOn' ':' 
  where 
    splitOn' :: Char -> String -> (String, String, String)
    splitOn' splitChar fullDependencyName = (groupId, artifactId, vers) 
      where
        groupId = takeWhile (/= splitChar) fullDependencyName :: String 
        artifactIdAndVersion = drop (length groupId + 1) fullDependencyName :: String
        artifactId = takeWhile (/= splitChar) artifactIdAndVersion :: String
        vers = drop (length artifactId + 1) artifactIdAndVersion :: String

extractLatestVersion :: MavenResponse -> String 
extractLatestVersion = version . head . docs . response

toNameTree :: DependencyTree -> String
toNameTree (DependencyTree name _ _) = name

dependencyTreeToNameTree :: DependencyTree -> NameTree
dependencyTreeToNameTree (DependencyTree name lvl subDeps) = 
    NameTree name lvl (map dependencyTreeToNameTree subDeps)

dependencyTreeToVersionTree :: DependencyTree -> VersionTree 
dependencyTreeToVersionTree (DependencyTree name lvl subDeps) = 
    VersionTree gav latest lvl (map dependencyTreeToVersionTree subDeps)
    where 
      gav = extractGAV name :: (String, String, String)
      latest = "" :: String

-- get the latest version of a dependency and recursively update the tree 
getLatestVersion :: VersionTree -> IO VersionTree 
getLatestVersion (VersionTree gav _ lvl subDeps) = do 
    let uri = mapToUri gav :: String
    request <- parseRequest uri :: IO Request
    let request' = setRequestHeader "User-Agent" ["MyCustomUserAgent/1.0"] request 
    response' <- httpLBS request' 
    let jsonResponse = eitherDecode $ getResponseBody response' :: Either String MavenResponse 

    updatedSubDeps <- mapM getLatestVersion subDeps
    case jsonResponse of 
      Left _ -> do 
        return $ VersionTree gav "Error" lvl updatedSubDeps :: IO VersionTree 
      Right _ -> do
        let newestVersion = either show extractLatestVersion jsonResponse :: String
        let updatedTree = VersionTree gav newestVersion lvl updatedSubDeps :: VersionTree
        return updatedTree :: IO VersionTree

mapToUri :: (String, String, String) -> String 
mapToUri (g, a, _) = "https://search.maven.org/solrsearch/select?q=g:" ++ g ++ "+AND+a:" ++ a ++ "&core=gav&rows=20&wt=json" 

flattenVersionTree :: VersionTree -> [VersionRecord]
flattenVersionTree (VersionTree (groupId, artifactId, currentVersion) latestVersion indentationLevel children) =
  VersionRecord groupId artifactId currentVersion latestVersion indentationLevel :
  concatMap flattenVersionTree children

writeVersionTreeToCSV :: FilePath -> [VersionTree] -> IO ()
writeVersionTreeToCSV filePath trees = do
  let records = concatMap flattenVersionTree trees
      csvData = encodeDefaultOrderedByName records -- If you need headers
      -- Or use `encode records` if you don't need headers
  BL.writeFile filePath csvData

