-- haskell statistics
-- scott banyard 28/01/16

import Data.List

-- finds mean by summing list and divding by number of elements
mean :: Fractional a => [a] -> a
mean xs = sum xs / fromIntegral(length xs)

-- create list of each element - m ^ 2
deviation :: Fractional a => [a] -> [a]
deviation xs = map (\x -> (x - m) ^ 2) xs
  where
    m = mean xs

-- sum of the deviations added up
sumdeviation :: Fractional a => [a] -> a
sumdeviation xs = sum $ deviation xs
-- sumdeviation = (sum . deviation) : can be written like this

-- sample variance = sum of each element - mean squared
variance :: Fractional a => [a] -> a
variance xs = sumdeviation xs / n
  where
    n = fromIntegral $ length xs - 1

-- square root of sample variance
standarddeviation ::  [Float] -> Float
standarddeviation xs = sqrt $ variance xs

-- median using Maybe data type - sorts list and then uses med as sub routine
median :: (Fractional a, Ord a) => [a] -> Maybe a
median xs = med $ sort xs
-- median = (med . sort) : can be written like this

-- subroutine for median - finds middle number depending on even/odd
med :: Fractional a => [a] -> Maybe a
med xs
  | null xs   = Nothing
  | odd  l    = Just $ xs !! mid
  | even l    = Just $ findMid
    where
      l = length xs
      mid = (l `div` 2)
      findMid = (xs !! mid + xs !! (mid-1)) / 2
