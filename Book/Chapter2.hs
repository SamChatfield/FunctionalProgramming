double x = x + x

quadruple x = double (double x)

-- Factorial of a positive integer
factorial n = product [1..n]

-- Average of a list of integers
average ns = sum ns `div` length ns

-- N = a 'div' length xs
n = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]

-- laste [] = []
-- laste (x:[]) = x
-- laste (_:xs) = laste xs

last1 xs = xs !! (length xs - 1)

last2 xs = head (reverse xs)

init1 xs = take (length xs - 1) xs

init2 xs = reverse (drop 1 (reverse xs))