import MorseLib
import TestCases
import Data.List
import Data.Tuple

{-
    ==========
    QUESTION 1
    ==========
-}

-- Encode a single word
codeWord :: String -> [MorseUnit]
codeWord []     = []
codeWord (c:cs) = codeSymbol c ++ shortGap ++ codeWord cs

-- Encode a list of words
codeText :: [String] -> [MorseUnit]
codeText []     = []
codeText (s:[]) = codeWord s
codeText (s:ss) = codeWord s ++ mediumGap ++ codeText ss

-- Encode a whole sentence
encode :: String -> [MorseUnit]
encode [] = []
encode s  = codeText (words s)

-- Test encode against the test cases
-- NOTE: needed to add "module TestCases where" to the top of TestCases.hs in order
--       be able to import the file and use the test cases
-- Return a list of True or False whether each part of the test passes
testEncode :: [String] -> [[MorseUnit]] -> [Bool]
testEncode [] []         = []
testEncode (i:is) (o:os) = (encode i == o) : testEncode is os

{-
    ==========
    QUESTION 2
    ==========
-}

-- Extract the next word from a sequence of morse code
nextWord :: [MorseUnit] -> [MorseUnit]
nextWord []          = []
nextWord (Silence:
          Silence:
          Silence:
          Silence:
          Silence:
          Silence:
          Silence:ms) = [Silence, Silence, Silence]
nextWord (Silence:
          Silence:
          Silence:[]) = [Silence, Silence, Silence]
nextWord (m:ms)       = m : nextWord ms

-- From a morse code sequence, return a list of the individual words' sequences
extractWords :: [MorseUnit] -> [[MorseUnit]]
extractWords [] = []
extractWords ms = nxt : extractWords (drop (length nxt + 4) ms)
                  where nxt = nextWord ms

-- Extract the next letter from a sequence of morse code that is a single word
nextLetter :: [MorseUnit] -> [MorseUnit]
nextLetter [] = []
nextLetter (Silence:
            Silence:
            Silence:ms) = [Silence]
nextLetter (m:ms) = m : nextLetter ms

-- From a morse sequence that is a single word, extract the component letters as a list
extractLetters :: [MorseUnit] -> [[MorseUnit]]
extractLetters [] = []
extractLetters ms = nxt : extractLetters (drop (length nxt + 2) ms)
                    where nxt = nextLetter ms

-- Use the table to look up a morse letter and return the associated character
retrieve :: [MorseUnit] -> MorseTable -> Char
retrieve m []     = '-'
retrieve c (m:ms) = if c == fst m then snd m else retrieve c ms

-- Construct a word from a list of individual letters (as returned from extractLetters)
consWord :: [[MorseUnit]] -> String
consWord []     = ""
consWord (l:ls) = (retrieve l table) : consWord ls

-- Auxilliary function that uses the list of individual word sequences of morse code.
-- Builds up the string recursively
decode' :: [[MorseUnit]] -> String
decode' [w]    = consWord (extractLetters w)
decode' (w:ws) = (consWord (extractLetters w)) ++ " " ++ decode' ws

-- Take the single long morse sequence and decode it to English
decode :: [MorseUnit] -> String
decode ms = decode' (extractWords ms)

-- Test decode against some test cases as in: testDecode input output
-- Assumption that test cases entered are valid e.g. length input == length output
testDecode :: [[MorseUnit]] -> [String] -> [Bool]
testDecode [] [] = []
testDecode (i:is) (o:os) = (decode i == o) : testDecode is os

{-
    ==========
    QUESTION 3
    ==========
-}

-- Tests if a given sequence of morse correpsonds to a character
isChar :: [MorseUnit] -> MorseTable -> Bool
isChar _ [] = False
isChar ms (t:ts) = if ms == fst t then True else isChar ms ts

toTree' :: MorseTable -> [MorseUnit] -> MorseTree
toTree' [] path = if isChar path table then Leaf (retrieve path table) else Nil
toTree' [m] path = if isChar path table then Leaf (snd m) else Branch0 left right
                   where left = toTree' [(drop 2 (fst l), snd l) | l <- [m], take 2 (fst l) == dit] (path ++ dit)
                         right = toTree' [(drop 4 (fst r), snd r) | r <- [m], take 4 (fst r) == dah] (path ++ dah)
toTree' ms path = if isChar path table then
                      Branch1 (retrieve path table) left right
                  else
                      Branch0 left right
                  where left = toTree' [(drop 2 (fst l), snd l) | l <- ms, take 2 (fst l) == dit] (path ++ dit)
                        right = toTree' [(drop 4 (fst r), snd r) | r <- ms, take 4 (fst r) == dah] (path ++ dah)

-- list comp dit left dah right
toTree :: MorseTable -> MorseTree
toTree ms = toTree' ms []

testToTree :: [MorseTable] -> [MorseTree] -> [Bool]
testToTree [] [] = []
testToTree (i:is) (o:os) = (toTree i == o) : testToTree is os

testTable :: MorseTable
testTable = [(dah, 'J'), ([], 'W'), (dah ++ dah, '1')]





























