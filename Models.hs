module Models where

data Phoneme = AA Emphasis | AE Emphasis | AH Emphasis | AO Emphasis |
    AW Emphasis | AY Emphasis | B | CH | D | DH | EH Emphasis | ER Emphasis |
    EY Emphasis | F | G | HH | IH Emphasis | IY Emphasis | JH | K | L | M |
    N | NG | OW Emphasis | OY Emphasis | P | R | S | SH | T | TH |
    UH Emphasis | UW Emphasis | V | W | Y | Z | ZH
    deriving (Eq,Show)

data Emphasis = Emp2 | Emp1 | Emp0 | EmpNone
    deriving (Eq, Show)

data SpelledWord = SpelledWord String [Phoneme]
    deriving (Eq,Show)

