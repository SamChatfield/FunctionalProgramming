--  CHAPTER 2
-- 1
double x = x + x
quadruple x = double (double x)
factorial n = product [1..n]
average ns = sum ns `div` length ns

-- 2
{-
    (2^3)*4
    (2*3)+(4*5)
    2+(3*(4^5))
-}

-- 3
n = a `div` length xs
    where
        a = 10
        xs = [1..5]

-- 4
last1 xs = xs !! (length xs - 1)
last2 xs = head (reverse xs)

-- 5
init1 xs = take (length xs - 1) xs
init2 xs = reverse (drop 1 (reverse xs))


-- CHAPTER 3
-- 1
{-
    ['a', 'b', 'c'] :: [Char]
    ('a', 'b', 'c') :: (Char, Char, Char)
    [(False, '0'), (True, '1')] :: [(Bool, Char)]
    ([False, True], ['0', '1']) :: ([Bool], [Char])
    [tail, init, reverse] :: [[a] -> [a]]
-}

-- 2
bools = [True,True,False]
nums = [[1,2,3],[4,5,6],[7,8,9]]
add a b c = a + b + c
copy a = (a,a)
apply f x = f x

-- 3
{-
    second :: [a] -> a
    swap :: (a,b) -> (b,a)
    pair :: a -> b -> (a,b)
    double :: Num a => a -> a
    palindrome :: Eq a => [a] -> Bool
    twice :: (a -> a) -> a -> a
-}

-- 4
second xs = head (tail xs)
swap (x,y) = (y,x)
pair x y = (x,y)
-- double x = x*2 -- commented otherwise double is defined twice and the script won't compile
palindrome xs = reverse xs == xs
twice f x = f (f x)

-- 5
{-
    To find out whether two functions are equal one would need to be convinced that they will
    always return the same result for all inputs which will obviously take a very long time.
    Therefore it is not usually feasible for function types to be a member of the Eq type class.
-}


-- CHAPTER 4
-- 1
halve xs = (take n xs, drop n xs)
           where n = length xs `div` 2

-- 2
third1 xs = head (tail (tail xs))
third2 xs = xs !! 2
third3 (_:_:x:_) = x

-- 3
safetail1 xs = if null xs then [] else tail xs

safetail2 xs | xs == []  = []
             | otherwise = tail xs

safetail3 [] = []
safetail3 xs = tail xs

-- 4
(||) :: Bool -> Bool -> Bool
True  || True  = True
True  || False = True
False || True  = True
False || False = False


-- CHAPTER 5
-- 1
hundredSquares = sum [x^2 | x <- [1..100]]

-- 2
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

-- 3
square n = [(x,y) | (x,y) <- grid n n, x /= y]

-- 4
replicate2 n x = [x | _ <- [1..n]]

-- 5
pyths n = [(x,y,z) | x <- ns, y <- ns, z <- ns, x^2 + y^2 == z^2]
          where ns = [1..n]
