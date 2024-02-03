module Gradle.SanitiserTest 
( runGradleSanitiserTest
) where

import Gradle.Sanitiser (keywords, replaceWithKeyword, replaceLineWithKeyword, groupByKeyword, sanitiseInput) 

import Test.QuickCheck
import Test.Hspec

runGradleSanitiserTest :: Spec
runGradleSanitiserTest = 
  describe "Gradle.Sanitiser" $ do
    replaceLineWithKeywordTest 
    runReplaceWithKeywordTest
    runGroupByKeywordTest
    runSanitiseInputTest

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

runReplaceWithKeywordTest :: Spec
runReplaceWithKeywordTest = 
  describe "runReplaceWithKeyword" $ do 
    context "When given an Empty List of strings" $ do 
      it "It should return an empty list" $ do 
        replaceWithKeyword keywords [] `shouldBe` []

    context "When given a list of all starting with a keyword" $ do
      it "It should return a list of all keywords" $ do 
        let input = ["implementation some test", "runtimeClasspath ", "testImplementation some text"]
        let expected = ["implementation", "runtimeClasspath", "testImplementation"]
        replaceWithKeyword keywords input `shouldBe` expected

    context "When given a list of all not starting with a keyword" $ do 
      it "It should return the same list" $ do 
        let input = ["not a keyword", "not a keyword", "not a keyword"]
        replaceWithKeyword keywords input `shouldBe` input

    context "When given a list of mixed strings" $ do 
      it "It should return a list of mixed strings some of which are keywords" $ do 
        let input = [ "not a keyword"
                    , "implementation some test"
                    , "not a keyword"
                    , "runtimeClasspath "
                    , "testImplementation some text"
                    ]
        let expected = [ "not a keyword"
                       , "implementation"
                       , "not a keyword"
                       , "runtimeClasspath"
                       , "testImplementation"
                       ]

        replaceWithKeyword keywords input `shouldBe` expected

runGroupByKeywordTest :: Spec 
runGroupByKeywordTest = 
  describe "groupByKeyword" $ do 
    context "When given an empty list" $ do 
      it "It should return an empty list" $ do 
        groupByKeyword keywords [] `shouldBe` []

    context "When given a list of all keywords" $ do 
      it "It should return a list of lists of length 1" $ do 
        let input = ["implementation", "runtimeClasspath", "testImplementation"]
        let expected = [ ["implementation"]
                       , ["runtimeClasspath"]
                       , ["testImplementation"]
                       ]
        groupByKeyword keywords input `shouldBe` expected

    context "When given a list of all non-keywords" $ do 
      it "It should return a list of list of length 1 with all strings in" $ do 
        let input = ["not a keyword", "not a keyword", "not a keyword"]
        let expected = [["not a keyword", "not a keyword", "not a keyword"]]
        groupByKeyword keywords input `shouldBe` expected

    context "When given a list of mixed strings" $ do 
      it "should group the strings by keyword" $ do 
        let input = [ "implementation"
                    , "not a keyword"
                    , "runtimeClasspath"
                    , "testImplementation"
                    , "not a keyword"
                    ]

        let expected = [ ["implementation", "not a keyword"]
                       , ["runtimeClasspath"] 
                       , ["testImplementation", "not a keyword"]
                       ]
        groupByKeyword keywords input `shouldBe` expected

runSanitiseInputTest :: Spec 
runSanitiseInputTest = 
  describe "sanitiseInput" $ do 
    context "When given an empty list" $ do 
      it "It should return an empty list" $ do 
        sanitiseInput keywords [] `shouldBe` []

    context "When given a list that starts with a keyword" $ do 
      it "It should return the same list" $ do 
        let input = ["implementation", "runtimeClasspath", "testImplementation"]
        sanitiseInput keywords input `shouldBe` input

    context "When given a list that does not start with a keyword" $ do 
      it "It should return an empty list" $ do 
        let input = ["not a keyword", "implementation", "not a keyword"]
        let expected = ["implementation", "not a keyword"]
        sanitiseInput keywords input `shouldBe` expected

    context "when given a list with no keywords" $ do 
      it "It should return the same list" $ do 
        let input = ["not a keyword", "not a keyword", "not a keyword"]
        sanitiseInput keywords input `shouldBe` []
