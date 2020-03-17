reduce_sum = foldl1 (+)

fib :: [Integer]
fib = 1 : 1 : zipWith (+) fib (tail fib)

solution = reduce_sum $ filter even $ takeWhile (<4000000) fib
