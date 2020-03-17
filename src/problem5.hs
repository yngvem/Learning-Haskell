import Data.List 
import Data.Ord (comparing)
import Data.Function (on)


flatten = intercalate []


divisible_by :: (Integral a) => a -> a -> Bool
divisible_by n m = m `mod` n == 0
is_factor n m = n `mod` m == 0

sieve :: (Integral a) => a -> [a] -> [a]
sieve n numbers = filter (not . divisible_by n) numbers

eratosthenes_sieve :: (Integral a) => [a] -> [a]
eratosthenes_sieve [] = []
eratosthenes_sieve (x:xs) = x : eratosthenes_sieve (sieve x (xs))

prime_factors :: (Integral a) => a -> [a]
prime_factors 1 = []
prime_factors n = 
    let max_prime = floor $ sqrt $ fromIntegral n
    in 
        let factors = filter (is_factor n) (eratosthenes_sieve [2..max_prime])
        in
            if factors == []
            then [n]
            else let f = head factors in f : prime_factors (n `quot` f)


divisible_factors = [2..20]

all_factors = let flatten = intercalate []
              in nub $ flatten $ map (group . prime_factors) divisible_factors

largest_divisible_factors = 
    let sorted_factors = sortBy (comparing head) all_factors
    in 
        let grouped_factors = groupBy ((==) `on` head) sorted_factors
        in map (maximumBy (comparing length)) grouped_factors

solution = product $ flatten largest_divisible_factors

