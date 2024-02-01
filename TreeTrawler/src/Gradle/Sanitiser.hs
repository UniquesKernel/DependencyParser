{-# LANGUAGE CPP #-}
module Gradle.Sanitiser 
(
#ifdef TEST 
  replaceLineWithKeyword
, keywords
#endif
) where

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

