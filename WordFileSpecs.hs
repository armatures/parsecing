import WordFile
import Test.QuickCheck
import FunctionsAndTypesForParsing (parseWithEof)
import Text.Parsec.String (Parser)
import Text.Parsec (ParseError)
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


prop_lower_then_upper (LowerString ls) (UpperString us) =
    parseWithEof wordLine ( ls ++ " " ++ us ) == Right ( SpelledWord ls us )

tests:: [ Either ParseError SpelledWord ]
tests =
    [ ( parseWithEof wordLine "yow CHOW" ),
        ( parseWithEof wordLine "yow CHOW #" ),
        ( parseWithEof wordLine "yow CHOW # comment" ),
        ( parseWithEof wordLine "yow CHOW # comment'with_weird\" characters" )
    ]
