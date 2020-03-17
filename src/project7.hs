is_factor :: (Integral a) => a -> a -> Bool
is_factor n m = m `mod` n == 0

is_factor_of :: (Integral a) => a -> a -> Bool
is_factor_of = flip is_factor

is_prime :: (Integral a) => [a] -> a -> Bool
is_prime previous_primes n = 
    let is_relevant = (<= (floor $ sqrt $ fromIntegral n))
    in 
        let relevant_previous_primes = takeWhile is_relevant previous_primes
        in
            not $ any (is_factor_of n) relevant_previous_primes

get_primes :: (Integral a) => a -> [a]
get_primes 1 = [2]
get_primes 2 = [3, 2]
get_primes n =
    let previous_primes = get_primes (n-1)
    in 
        let is_next_prime = is_prime $ reverse previous_primes
            previous_prime = head previous_primes
        in 
            let next_possible_primes = [(previous_prime + 2), (previous_prime + 4)..]
            in head (filter is_next_prime next_possible_primes) : previous_primes 


solution = head $ get_primes 10001
