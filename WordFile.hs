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
import Models (SpelledWord(..), Phoneme(..), Emphasis(..))

dictionaryParser :: Parser [ SpelledWord ]
dictionaryParser = many1 wordLine

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
    spelling <- spellingParser
    string " "
    pronounciation <- pronounciationParser
    return (SpelledWord spelling pronounciation)

spellingParser =
    many1 $ noneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ "

pronounciationParser :: Parser [Phoneme]
pronounciationParser =
    many1 $ lexeme $ phonemeParser

phonemeParser :: Parser Phoneme
phonemeParser = do
    phoneme <- parsePhone
    return ( fromString phoneme )

--horrible function that doesn't handle error case
fromString :: String -> Phoneme
fromString s =
    case s of
      "AA" -> AA EmpNone
      "AA0" -> AA Emp0
      "AA1" -> AA Emp1
      "AA2" -> AA Emp2
      "AE" -> AE EmpNone
      "AE0" -> AE Emp0
      "AE1" -> AE Emp1
      "AE2" -> AE Emp2
      "AH" -> AH EmpNone
      "AH0" -> AH Emp0
      "AH1" -> AH Emp1
      "AH2" -> AH Emp2
      "AO" -> AO EmpNone
      "AO0" -> AO Emp0
      "AO1" -> AO Emp1
      "AO2" -> AO Emp2
      "AW" -> AW EmpNone
      "AW0" -> AW Emp0
      "AW1" -> AW Emp1
      "AW2" -> AW Emp2
      "AY" -> AY EmpNone
      "AY0" -> AY Emp0
      "AY1" -> AY Emp1
      "AY2" -> AY Emp2
      "B" -> B
      "CH" -> CH
      "DH" -> DH
      "EH" -> EH EmpNone
      "EH0" -> EH Emp0
      "EH1" -> EH Emp1
      "EH2" -> EH Emp2
      "ER" -> ER EmpNone
      "ER0" -> ER Emp0
      "ER1" -> ER Emp1
      "ER2" -> ER Emp2
      "EY" -> EY EmpNone
      "EY0" -> EY Emp0
      "EY1" -> EY Emp1
      "EY2" -> EY Emp2
      "F" -> F
      "G" -> G
      "IH" -> IH EmpNone
      "IH0" -> IH Emp0
      "IH1" -> IH Emp1
      "IH2" -> IH Emp2
      "IY" -> IY EmpNone
      "IY0" -> IY Emp0
      "IY1" -> IY Emp1
      "IY2" -> IY Emp2
      "JH" -> JH
      "K" -> K
      "L" -> L
      "N" -> N
      "NG" -> NG
      "OW" -> OW EmpNone
      "OW0" -> OW Emp0
      "OW1" -> OW Emp1
      "OW2" -> OW Emp2
      "OY" -> OY EmpNone
      "OY0" -> OY Emp0
      "OY1" -> OY Emp1
      "OY2" -> OY Emp2
      "P" -> P
      "R" -> R
      "S" -> S
      "SH" -> SH
      "T" -> T
      "TH" -> TH
      "UH" -> UH EmpNone
      "UH0" -> UH Emp0
      "UH1" -> UH Emp1
      "UH2" -> UH Emp2
      "UW" -> UW EmpNone
      "UW0" -> UW Emp0
      "UW1" -> UW Emp1
      "UW2" -> UW Emp2
      "V" -> V
      "W" -> W
      "Y" -> Y
      "Z" -> Z
      "ZH" -> ZH
      _ -> Z

parsePhone :: Parser String
parsePhone =
    many1 ( digit <|> upper )

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
