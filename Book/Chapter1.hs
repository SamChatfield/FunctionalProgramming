sum_x [] = 0
sum_x (x:xs) = x + sum xs

qsort []     = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                 smaller = [a | a <- xs, a <= x]
                 larger = [b | b <- xs, b > x]

seqn [] = return []
seqn (act:acts) = do x <- act
                     xs <- seqn acts
                     return (x:xs)

-- Exercise 3
prod [] = 1
prod (x:xs) = x * prod xs