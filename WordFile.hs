module WordFile where

import Text.Parsec (ParseError)
import Text.Parsec.String (Parser)
import Text.Parsec.String.Parsec (try)
import Text.Parsec.String.Char (oneOf, noneOf, char, digit, satisfy, string, lower, upper)
import Text.Parsec.String.Combinator (many1, choice, chainl1, sepBy1)
import Control.Applicative ((<|>), many)
import Control.Monad (void)
import Data.Char (isLetter, isDigit)
import FunctionsAndTypesForParsing

data SpelledWord = SpelledWord String String
    deriving (Eq,Show)

wordLine :: Parser SpelledWord
wordLine =
    try withComment <|> wordParser

withComment :: Parser SpelledWord
withComment = do
    w <- lexeme $ wordParser
    void $ lexeme $ char '#'
    void $ lexeme $ many $ noneOf "\n"
    return $ w

wordParser :: Parser SpelledWord
wordParser = do
    spelling <- many1 $ lower
    string " "
    pronounciation <- many1 $ upper
    return (SpelledWord spelling pronounciation)

lexeme :: Parser a -> Parser a
lexeme p = do
           x <- p
           whitespace
           return x

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

-- parseWithEof wordLine "yow CHOW"
-- wordLine "yow CHOW # comment"
-- quickCheck prop_lower_then_upper
-- parseWithEof ( sepBy1 (many1 $ satisfy (\_ -> True)) (string "  ")) $ "a t  e"
