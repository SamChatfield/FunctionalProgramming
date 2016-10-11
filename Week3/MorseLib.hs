module MorseLib where

data MorseUnit = Beep | Silence
  deriving (Eq, Show)

dit, dah, shortGap, mediumGap :: [MorseUnit]
dit       = [Beep, Silence]
dah       = [Beep, Beep, Beep, Silence]
shortGap  = replicate (3-1) Silence
mediumGap = replicate (7-3) Silence

codeSymbol :: Char -> [MorseUnit]
codeSymbol 'A' = dit ++ dah
codeSymbol 'B' = dah ++ dit ++ dit ++ dit
codeSymbol 'C' = dah ++ dit ++ dah ++ dit
codeSymbol 'D' = dah ++ dit ++ dit
codeSymbol 'E' = dit
codeSymbol 'F' = dit ++ dit ++ dah ++ dit
codeSymbol 'G' = dah ++ dah ++ dit
codeSymbol 'H' = dit ++ dit ++ dit ++ dit
codeSymbol 'I' = dit ++ dit
codeSymbol 'J' = dit ++ dah ++ dah ++ dah
codeSymbol 'K' = dah ++ dit ++ dah
codeSymbol 'L' = dit ++ dah ++ dit ++ dit
codeSymbol 'M' = dah ++ dah
codeSymbol 'N' = dah ++ dit
codeSymbol 'O' = dah ++ dah ++ dah
codeSymbol 'P' = dit ++ dah ++ dah ++ dit
codeSymbol 'Q' = dah ++ dah ++ dit ++ dah
codeSymbol 'R' = dit ++ dah ++ dit
codeSymbol 'S' = dit ++ dit ++ dit
codeSymbol 'T' = dah
codeSymbol 'U' = dit ++ dit ++ dah
codeSymbol 'V' = dit ++ dit ++ dit ++ dah
codeSymbol 'W' = dit ++ dah ++ dah
codeSymbol 'X' = dah ++ dit ++ dit ++ dah
codeSymbol 'Y' = dah ++ dit ++ dah ++ dah
codeSymbol 'Z' = dah ++ dah ++ dit ++ dit
codeSymbol '1' = dit ++ dah ++ dah ++ dah ++ dah
codeSymbol '2' = dit ++ dit ++ dah ++ dah ++ dah
codeSymbol '3' = dit ++ dit ++ dit ++ dah ++ dah
codeSymbol '4' = dit ++ dit ++ dit ++ dit ++ dah
codeSymbol '5' = dit ++ dit ++ dit ++ dit ++ dit
codeSymbol '6' = dah ++ dit ++ dit ++ dit ++ dit
codeSymbol '7' = dah ++ dah ++ dit ++ dit ++ dit
codeSymbol '8' = dah ++ dah ++ dah ++ dit ++ dit
codeSymbol '9' = dah ++ dah ++ dah ++ dah ++ dit
codeSymbol '0' = dah ++ dah ++ dah ++ dah ++ dah

type MorseTable = [([MorseUnit], Char)]

table :: MorseTable
table = [(dit ++ dah, 'A'),
         (dah ++ dit ++ dit ++ dit, 'B'),
         (dah ++ dit ++ dah ++ dit, 'C'),
         (dah ++ dit ++ dit, 'D'),
         (dit, 'E'),
         (dit ++ dit ++ dah ++ dit, 'F'),
         (dah ++ dah ++ dit, 'G'),
         (dit ++ dit ++ dit ++ dit, 'H'),
         (dit ++ dit, 'I'),
         (dit ++ dah ++ dah ++ dah, 'J'),
         (dah ++ dit ++ dah, 'K'),
         (dit ++ dah ++ dit ++ dit, 'L'),
         (dah ++ dah, 'M'),
         (dah ++ dit, 'N'),
         (dah ++ dah ++ dah, 'O'),
         (dit ++ dah ++ dah ++ dit, 'P'),
         (dah ++ dah ++ dit ++ dah, 'Q'),
         (dit ++ dah ++ dit, 'R'),
         (dit ++ dit ++ dit, 'S'),
         (dah, 'T'),
         (dit ++ dit ++ dah, 'U'),
         (dit ++ dit ++ dit ++ dah, 'V'),
         (dit ++ dah ++ dah, 'W'),
         (dah ++ dit ++ dit ++ dah, 'X'),
         (dah ++ dit ++ dah ++ dah, 'Y'),
         (dah ++ dah ++ dit ++ dit, 'Z'),
         (dit ++ dah ++ dah ++ dah ++ dah, '1'),
         (dit ++ dit ++ dah ++ dah ++ dah, '2'),
         (dit ++ dit ++ dit ++ dah ++ dah, '3'),
         (dit ++ dit ++ dit ++ dit ++ dah, '4'),
         (dit ++ dit ++ dit ++ dit ++ dit, '5'),
         (dah ++ dit ++ dit ++ dit ++ dit, '6'),
         (dah ++ dah ++ dit ++ dit ++ dit, '7'),
         (dah ++ dah ++ dah ++ dit ++ dit, '8'),
         (dah ++ dah ++ dah ++ dah ++ dit, '9'),
         (dah ++ dah ++ dah ++ dah ++ dah, '0')]


data MorseTree = Nil 
               | Leaf Char 
               | Branch1 Char MorseTree MorseTree 
               | Branch0 MorseTree MorseTree 
  deriving (Eq, Show)

