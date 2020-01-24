--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab: List comprehensions                                                   --
--------------------------------------------------------------------------------

module Lab where

--------------------------------------------------------------------------------

-- 1. Using the [n..m] syntax for lists, define a list of the numbers 0 to 10.
zeroToTen :: (Num a, Enum a) => [a]
zeroToTen = [0..10]

-- 2. Using the [n..m] syntax for lists, define a list of the numbers 4 to 8.
fourToEight :: (Num a, Enum a) => [a]
fourToEight = [4..8]

-- 3. Using the [n..m] syntax for lists, define a list of the lower-case
-- characters from 'a' to 'z'.
lowercase :: [Char]
lowercase = ['a'..'z']

-- 4. Using a list comprehension, define a list of the powers of two for
-- the exponents from 1 to 10.
powersOfTwo :: Num a => [a]
powersOfTwo = [2^n | n <- [1..10]]

-- 5. Using a list comprehension, define a list of the factorials of the
-- numbers 1 to 10.
factorials :: [Int]
factorials = [fac n | n <- [1..10]]
    where fac n = product [1..n]

-- 6. Using a list comprehension, define a list of coordinates where the top
-- left corner is (0,0) and the bottom right is (10,10).
coords :: (Num a, Enum a) => [(a,a)]
coords = [(x,y) | x <- [0..10], y <- [0..10]]

-- 7. Using a list comprehension, define a list of all pairs where both sections
-- are in the range from 0 to 5, such that the sum of both sections never
-- exceeds 5.
noMoreThanFive :: (Num a, Enum a, Ord a) => [(a,a)]
noMoreThanFive = [(x,y) | x <- [0..5], y <- [0..5], x + y <= 5]

noMoreThanFive' :: (Num a, Enum a) => [(a,a)]
noMoreThanFive' = [(x,y) | x <- [0..5], y <- [0..(5-x)]]

-- 8. Using a list comprehension with a predicate, define a list of all even
-- numbers from 0 to 100.
evens :: [Int]
evens = [n | n <- [0..100], even n]

-- 9. Write a function which, given some natural number n, produces a list of
-- all multiples of 3 and 5 in the range of 0 to n (inclusive). For example,
-- multiples 10 = [0,3,5,6,9,10]
multiples :: Int -> [Int]
multiples n = [x | x <- [0..n], mod x 3 == 0 || mod x 5 == 0]

--------------------------------------------------------------------------------
