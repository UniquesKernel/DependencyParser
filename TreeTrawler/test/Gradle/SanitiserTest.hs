module Gradle.SanitiserTest 
( runGradleSanitiserTest
) where

import Gradle.Sanitiser (replaceLineWithKeyword, keywords)

import Test.Hspec
import Test.QuickCheck

runGradleSanitiserTest :: Spec
runGradleSanitiserTest = 
  describe "Gradle.Sanitiser" $ do
    replaceLineWithKeywordTest 

replaceLineWithKeywordTest :: Spec 
replaceLineWithKeywordTest = 
  describe "replaceLineWithKeyword" $ do 
    context "When given a string starting with a keyword" $ do 
      it "returns the keyword" $ do 
        replaceLineWithKeyword keywords "implementation" `shouldBe` "implementation"

    context "When given an empty string" $ do 
      it "returns the empty string" $ do 
        replaceLineWithKeyword keywords "" `shouldBe` ""

    context "When given a string not starting with a keyword" $ do 
      it "returns the string" $ do 
        replaceLineWithKeyword keywords "not a keyword" `shouldBe` "not a keyword"

    context "When given a string starting with a keyword and other text" $ do 
      it "it should always return the keyword" $ do 
        quickCheck alwaysReturnKeywordProp

    context "When given a string starting with a keyword and other text" $ do 
      it "it should always return a one word string" $ do 
        quickCheck alwaysReturnStringOfLength1

-- make a generator that generates strings that start with a keyword 
keywordStringGen :: Gen String 
keywordStringGen = do 
  keyword <- elements keywords 
  rest <- arbitrary 
  return $ keyword ++ " " ++ rest

-- make a property ensuring that the sanitiser always returns a string that is one of the keywords 
alwaysReturnKeywordProp :: Property 
alwaysReturnKeywordProp = 
  forAll keywordStringGen $ \str -> 
    replaceLineWithKeyword keywords str `elem` keywords 

alwaysReturnStringOfLength1 :: Property
alwaysReturnStringOfLength1 = 
  forAll keywordStringGen $ \str -> 
    (length . words . replaceLineWithKeyword keywords) str == 1
