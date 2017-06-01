module WordFile where

import Text.Parsec (ParseError)
import Text.Parsec.String (Parser)
import Text.Parsec.String.Parsec (try)
import Text.Parsec.String.Char (oneOf, char, digit, satisfy, string, lower, upper)
import Text.Parsec.String.Combinator (many1, choice, chainl1, sepBy1)
import Control.Applicative ((<|>), many)
import Control.Monad (void)
import Data.Char (isLetter, isDigit)
import FunctionsAndTypesForParsing

data SpelledWord = SpelledWord String String
    deriving (Eq,Show)

wordLine :: Parser SpelledWord
wordLine = do
    spelling <- many1 $ lower
    string " "
    pronounciation <- many1 $ upper
    return (SpelledWord spelling pronounciation)



-- parseWithEof wordLine "yow CHOW"
-- quickCheck prop_lower_then_upper
-- parseWithEof ( sepBy1 (many1 $ satisfy (\_ -> True)) (string "  ")) $ "a t  e"
