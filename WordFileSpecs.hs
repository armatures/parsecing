import WordFile
import Test.QuickCheck
import FunctionsAndTypesForParsing (parseWithEof)
import Text.Parsec.String (Parser)
import Text.Parsec (ParseError)
import Models (SpelledWord(..), Phoneme)
-- import Text.Parsec.String.Char (anyChar)
-- import Text.Parsec.String.Char
-- import FunctionsAndTypesForParsing (regularParse, parseWithEof, parseWithLeftOver)
-- import Data.Char
-- import Text.Parsec.String.Combinator (many1)


lowerChar :: Gen Char
lowerChar = elements ['a'..'z']

newtype LowerString = LowerString { unwrapLowerString :: String }
    deriving Show

instance Arbitrary LowerString where
    arbitrary = LowerString <$> listOf1 lowerChar


upperChar :: Gen Char
upperChar = elements ['A'..'Z']

newtype UpperString = UpperString { unwrapUpperString :: String }
    deriving Show

instance Arbitrary UpperString where
    arbitrary = UpperString <$> listOf1 upperChar


tests:: [ Either ParseError SpelledWord ]
tests =
    [ ( parseWithEof wordLine "yow CH OW" ),
        ( parseWithEof wordLine "yow Y OW #" ),
        ( parseWithEof wordLine "yow Y OW # comment" ),
        ( parseWithEof wordLine "yew-chow Y UW2 CH OW1 # dash in spelling" ),
        ( parseWithEof wordLine "yow CH OW # comment'with_weird\" characters" )
    ]
