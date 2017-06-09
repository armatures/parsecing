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
import Models (SpelledWord(..), Phoneme(..))

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
      "AA" -> AA
      "AA0" -> AA0
      "AA1" -> AA1
      "AA2" -> AA2
      "AE" -> AE
      "AE0" -> AE0
      "AE1" -> AE1
      "AE2" -> AE2
      "AH" -> AH
      "AH0" -> AH0
      "AH1" -> AH1
      "AH2" -> AH2
      "AO" -> AO
      "AO0" -> AO0
      "AO1" -> AO1
      "AO2" -> AO2
      "AW" -> AW
      "AW0" -> AW0
      "AW1" -> AW1
      "AW2" -> AW2
      "AY" -> AY
      "AY0" -> AY0
      "AY1" -> AY1
      "AY2" -> AY2
      "B" -> B
      "CH" -> CH
      "DH" -> DH
      "EH" -> EH
      "EH0" -> EH0
      "EH1" -> EH1
      "EH2" -> EH2
      "ER" -> ER
      "ER0" -> ER0
      "ER1" -> ER1
      "ER2" -> ER2
      "EY" -> EY
      "EY0" -> EY0
      "EY1" -> EY1
      "EY2" -> EY2
      "F" -> F
      "G" -> G
      "IH" -> IH
      "IH0" -> IH0
      "IH1" -> IH1
      "IH2" -> IH2
      "IY" -> IY
      "IY0" -> IY0
      "IY1" -> IY1
      "IY2" -> IY2
      "JH" -> JH
      "K" -> K
      "L" -> L
      "N" -> N
      "NG" -> NG
      "OW" -> OW
      "OW0" -> OW0
      "OW1" -> OW1
      "OW2" -> OW2
      "OY" -> OY
      "OY0" -> OY0
      "OY1" -> OY1
      "OY2" -> OY2
      "P" -> P
      "R" -> R
      "S" -> S
      "SH" -> SH
      "T" -> T
      "TH" -> TH
      "UH" -> UH
      "UH0" -> UH0
      "UH1" -> UH1
      "UH2" -> UH2
      "UW" -> UW
      "UW0" -> UW0
      "UW1" -> UW1
      "UW2" -> UW2
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
