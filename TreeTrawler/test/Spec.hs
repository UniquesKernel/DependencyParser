import Gradle.LexerTest (runLexerTest)
import Gradle.ParserTest (runParserTest)
import Gradle.SanitiserTest (runGradleSanitiserTest)

import Test.Hspec 

main :: IO ()
main = hspec $ do 
  runLexerTest  
  runParserTest
  runGradleSanitiserTest
