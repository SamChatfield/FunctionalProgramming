import Data.List

type FreqTable = [(Char,Int)]

freqs :: String -> FreqTable
freqs [] = []
freqs (x:xs) = (x, length (filter (==x) xs) + 1) : freqs (filter (/=x) xs)

order :: FreqTable -> FreqTable
order xs = sort xs