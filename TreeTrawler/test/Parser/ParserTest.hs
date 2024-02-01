module Parser.ParserTest 
( runParserTest 
) where

import Test.Hspec 
import Text.Megaparsec 
import Parser.Parser 
import Lexer.Lexer

newtype TTokenWrapper = TTokenWrapper { unwrapTToken :: TToken }
  deriving (Eq, Show)

runParserTest :: Spec 
runParserTest = 
    pDependencyTreeRootTest 

pDependencyTreeRootTest :: Spec 
pDependencyTreeRootTest = 
  describe "Parser" $ do 
    context "When given a single TToken" $ do
      it "should return a dependencyTree with no branches" $ do 
        let input = [(0, "rootDependency")] :: [TToken]
        let expected = DependencyTree "rootDependency" 0 [] :: DependencyTree

        case parse pDependencyTree "" input of 
          Left err -> expectationFailure $ show err
          Right actual -> actual `shouldBe` expected

    context "When given a single TToken with a child" $ do 
      it "should return a dependencyTree with a single branch" $ do 
        let input = [(0, "rootDependency"), (1, "childDependency")] :: [TToken]
        let expected = DependencyTree "rootDependency" 0 [DependencyTree "childDependency" 1 []] :: DependencyTree

        case parse pDependencyTree "" input of 
          Left err -> expectationFailure $ show err
          Right actual -> actual `shouldBe` expected

    context "When given a single TToken with a child and a grandchild" $ do 
      it "should return a dependencyTree with a single branch with a single branch" $ do 
        let input = [(0, "rootDependency"), (1, "childDependency"), (2, "grandchildDependency")] :: [TToken]
        let expected = DependencyTree "rootDependency" 0 [DependencyTree "childDependency" 1 [DependencyTree "grandchildDependency" 2 []]] :: DependencyTree

        case parse pDependencyTree "" input of 
          Left err -> expectationFailure $ show err
          Right actual -> actual `shouldBe` expected

    context "When give two TTokens with the same level" $ do 
      it "should return a dependencyTree with two branches" $ do 
        let input = [(0, "rootDependency"), (1, "child1"), (1, "child2")] :: [TToken]
        let expected = DependencyTree "rootDependency" 0 [DependencyTree "child1" 1 [], DependencyTree "child2" 1 []] :: DependencyTree

        case parse pDependencyTree "" input of 
          Left err -> expectationFailure $ show err
          Right actual -> actual `shouldBe` expected

      
