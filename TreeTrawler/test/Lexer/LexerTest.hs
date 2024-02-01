module Lexer.LexerTest 
( runLexerTest
) where

import Test.Hspec
import Lexer.Lexer 
import Text.Megaparsec
import Test.QuickCheck

runLexerTest :: Spec 
runLexerTest = do 
  describe "Lexer" $ do
    lexemeTest
    pSpaceTest 
    pPipeTest
    pPlusTest 
    pBackslashTest
    pIndentTest
    pDependencyTest
    pSpaceIndentTest

lexemeTest :: Spec
lexemeTest = 
  describe "lexeme" $ do 
    context "when given a root dependency" $ do 
      it "should return the dependency with indentation 0" $ do 
        parse lexeme "" "org.apache.commons:commons-lang3:3.12.0" `shouldBe` Right [(0, "org.apache.commons:commons-lang3:3.12.0")]

    context "when given a direct dependency" $ do 
      it "should return the dependency with indentation 1" $ do 
        parse lexeme "" "| org.apache.commons:commons-lang3:3.12.0" `shouldBe` Right [(1, "org.apache.commons:commons-lang3:3.12.0")]

    context "when given a direct dependency with a plus" $ do 
      it "should return the dependency with indentation 1" $ do 
        parse lexeme "" "+--- org.apache.commons:commons-lang3:3.12.0" `shouldBe` Right [(1, "org.apache.commons:commons-lang3:3.12.0")]

    context "when given a direct dependency with a backslash" $ do 
      it "should return the dependency with indentation 1" $ do 
        parse lexeme "" "\\--- org.apache.commons:commons-lang3:3.12.0" `shouldBe` Right [(1, "org.apache.commons:commons-lang3:3.12.0")]

    context "when given a transitive depdendency" $ do 
      it "should return the dependency with indentation 2 or more" $ do 
        parse lexeme "" "| | org.apache.commons:commons-lang3:3.12.0" `shouldBe` Right [(2, "org.apache.commons:commons-lang3:3.12.0")]

    context "when given a transitive depdendency with a plus" $ do 
      it "should return the dependency with indentation 2 or more" $ do 
        parse lexeme "" "| +--- org.apache.commons:commons-lang3:3.12.0" `shouldBe` Right [(2, "org.apache.commons:commons-lang3:3.12.0")] 

    context "when given a transitive depdendency with a backslash" $ do
      it "should return the dependency with indentation 2 or more" $ do 
        parse lexeme "" "| \\--- org.apache.commons:commons-lang3:3.12.0" `shouldBe` Right [(2, "org.apache.commons:commons-lang3:3.12.0")]

pSpaceTest :: Spec 
pSpaceTest = 
  describe "pSpace" $ do 
    context "when given a space" $ do 
      it "should return ()" $ do 
        parse pSpace "" " " `shouldBe` Right ()

    context "when given a tab" $ do 
      it "should return ()" $ do 
        parse pSpace "" "\t" `shouldBe` Right ()

    context "when given a newline" $ do 
      it "should return ()" $ do 
        parse pSpace "" "\n" `shouldBe` Right ()

    context "when given a carriage return" $ do 
      it "should return ()" $ do 
        parse pSpace "" "\r" `shouldBe` Right ()

pPipeTest :: Spec 
pPipeTest = 
  describe "pPipe" $ do 
    context "when given a pipe" $ do 
      it "should return 1" $ do 
        parse pPipe "" "|" `shouldBe` Right 1

    context "when given a pipe followed by a space" $ do 
      it "should return 1" $ do 
        parse pPipe "" "| " `shouldBe` Right 1

    context "when given a pipe followed by a tab" $ do 
      it "should return 1" $ do 
        parse pPipe "" "|\t" `shouldBe` Right 1

    context "when given a pipe followed by a newline" $ do 
      it "should return 1" $ do 
        parse pPipe "" "|\n" `shouldBe` Right 1

    context "when given a pipe followed by a carriage return" $ do 
      it "should return 1" $ do 
        parse pPipe "" "|\r" `shouldBe` Right 1

pPlusTest :: Spec 
pPlusTest = 
  describe "pPlus" $ do 
    context "when given a plus" $ do 
      it "should return 1" $ do 
        parse pPlus "" "+---" `shouldBe` Right 1

    context "when given a plus followed by a space" $ do 
      it "should return 1" $ do 
        parse pPlus "" "+--- " `shouldBe` Right 1

    context "when given a plus followed by a tab" $ do 
      it "should return 1" $ do 
        parse pPlus "" "+---\t" `shouldBe` Right 1

    context "when given a plus followed by a newline" $ do 
      it "should return 1" $ do 
        parse pPlus "" "+---\n" `shouldBe` Right 1

    context "when given a plus followed by a carriage return" $ do 
      it "should return 1" $ do 
        parse pPlus "" "+---\r" `shouldBe` Right 1

pBackslashTest :: Spec 
pBackslashTest = 
  describe "pBackslash" $ do 
    context "when given a backslash" $ do 
      it "should return 1" $ do 
        parse pBackslash "" "\\---" `shouldBe` Right 1

    context "when given a backslash followed by a space" $ do 
      it "should return 1" $ do 
        parse pBackslash "" "\\--- " `shouldBe` Right 1

    context "when given a backslash followed by a tab" $ do 
      it "should return 1" $ do 
        parse pBackslash "" "\\---\t" `shouldBe` Right 1

    context "when given a backslash followed by a newline" $ do 
      it "should return 1" $ do 
        parse pBackslash "" "\\---\n" `shouldBe` Right 1

    context "when given a backslash followed by a carriage return" $ do 
      it "should return 1" $ do 
        parse pBackslash "" "\\---\r" `shouldBe` Right 1

pSpaceIndentTest :: Spec 
pSpaceIndentTest = 
  describe "pSpaceIndent" $ do 
    context "when given a space" $ do 
      it "should return 1" $ do 
        parse pSpaceIndent "" "     " `shouldBe` Right 1

-- 
pIndentTest :: Spec 
pIndentTest = do
  describe "pIndent" $ do
    it "correctly counts the indentation sequences" $ do 
      quickCheck $ withMaxSuccess 10000 prop_pIndent

-- Generator for a mix of sequences
genIndentString :: Gen String
genIndentString = do 
  leadingSpaces <- listOf $ elements ["", "     "]
  sequences <- listOf genLine
  return $ concat leadingSpaces ++ concat sequences
  where
    genLine = do elements ["|", "+---", "\\---"]

prop_pIndent :: Property
prop_pIndent = forAll genIndentString $ \str ->
  case parse pIndent "" str of
    Left err -> counterexample (errorBundlePretty err) False
    Right result -> result === length (filter (`elem` ["|", "+---", "\\---", "     "]) (group str))

-- Helper function to group the sequences correctly
group :: String -> [String]
group [] = []
group s@(x:xs)
  | x == '|' = take 1 s : group (drop 1 s)
  | take 4 s == "+---" = take 4 s : group (drop 4 s)
  | take 4 s == "\\---" = take 4 s : group (drop 4 s)
  | take 5 s == "     " = take 5 s : group (drop 5 s)
  | otherwise = group xs

pDependencyTest :: Spec 
pDependencyTest = do 
  describe "pDependency" $ do 
    context "when given a dependency" $ do 
      it "should return the dependency" $ do 
        parse pDependency "" "org.apache.commons:commons-lang3:3.12.0" `shouldBe` Right "org.apache.commons:commons-lang3:3.12.0"

    context "when given a dependency with a space" $ do
      it "should return the dependency" $ do 
        parse pDependency "" "org.apache.commons:commons-lang3:3.12.0 " `shouldBe` Right "org.apache.commons:commons-lang3:3.12.0 "

    context "when given a dependency with a tab" $ do
      it "should return the dependency" $ do 
        parse pDependency "" "org.apache.commons:commons-lang3:3.12.0\t" `shouldBe` Right "org.apache.commons:commons-lang3:3.12.0\t"

    context "when given a dependency with a newline" $ do 
      it "should return the dependency without newline" $ do 
        parse pDependency "" "org.apache.commons:commons-lang3:3.12.0\n" `shouldBe` Right "org.apache.commons:commons-lang3:3.12.0"

    context "when given a dependency with a carriage return" $ do 
      it "should return the dependency" $ do 
        parse pDependency "" "org.apache.commons:commons-lang3:3.12.0\r" `shouldBe` Right "org.apache.commons:commons-lang3:3.12.0\r" 

    context "when given a dependency with a space, tab, newline, and carriage return" $ do 
      it "should return the dependency" $ do 
        parse pDependency "" "org.apache.commons:commons-lang3:3.12.0 \t\n\r" `shouldBe` Right "org.apache.commons:commons-lang3:3.12.0 \t"


