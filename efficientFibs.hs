-- Looked at this in tutorial 30/04/15

{-* Unefficient use of fibonacci. O(2^n) complexity. This is because everytime
	we call f, we create a new branch therefore splits into two, i.e. f10 -> f8 + f9.
	f8 will split into f6 + f7 [(n-2) + (n-1)] and so forth. *-}

f :: Int -> Int
f n
	| n < 2			= n
	| otherwise 	= f (n-1) + f (n-2)


{-**********-}

{-* Efficient use of fibonacci. O(n) complexity. Use of an infinity list (say an array) where
	we can just grab the fib values when needed. !! means look up in a list. -}

fib :: Int -> Integer
fib n = fibs!!(n-1)

-- n-1 because the list starts at 0,1,2.... so fib 3 would actually be fib 4 etc.

fibs :: [Integer]
fibs = 1:1:zipWith (+) fibs (tail fibs)
