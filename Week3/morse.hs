import MorseLib
import Data.List
import Data.Tuple

-- QUESTION 1

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

-- QUESTION 2

-- Extract the next word from a sequence of morse code
nextWord :: [MorseUnit] -> [MorseUnit]
nextWord []          = []
nextWord (Silence:
          Silence:
          Silence:
          Silence:
          Silence:
          Silence:
          Silence:ms) = [Silence]
nextWord (Silence:
          Silence:
          Silence:[]) = [Silence]
nextWord (m:ms)      = m : nextWord ms

-- From a morse code sequence, return a list of the individual words' sequences
extractWords :: [MorseUnit] -> [[MorseUnit]]
extractWords [] = []
extractWords ms = nxt : extractWords (drop (length nxt + 6) ms)
                  where nxt = nextWord ms

-- Use the table to look up a morse letter and return the associated character
-- lookup :: [MorseUnit] -> Char

-- decode :: [MorseUnit] -> String