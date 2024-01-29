import Test.Hspec 
import Lexer.LexerTest (runLexerTest)
import Parser.ParserTest (runParserTest)


main :: IO ()
main = hspec $ do 
  runLexerTest  
  runParserTest
