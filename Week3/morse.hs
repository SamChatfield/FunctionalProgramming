import MorseLib

-- Question 1
codeWord :: String -> [MorseUnit]
codeWord []     = []
codeWord (c:cs) = codeSymbol c ++ shortGap ++ codeWord cs

codeText :: [String] -> [MorseUnit]
codeText []     = []
codeText (s:[]) = codeWord s
codeText (s:ss) = codeWord s ++ mediumGap ++ codeText ss

encode :: String -> [MorseUnit]
encode [] = []
encode s  = codeText (words s)