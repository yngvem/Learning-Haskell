is_multiple_of_n :: Int -> Int -> Bool
is_multiple_of_n n x = x `mod` n == 0

is_multiple_of_3 = is_multiple_of_n 3
is_multiple_of_5 = is_multiple_of_n 5
is_multiple_of_3_or_5 n = is_multiple_of_3 n || is_multiple_of_5 n

reduce_sum :: [Int] -> Int -> Int
reduce_sum l acc = if (length l > 0) 
                   then reduce_sum (tail l) (acc + (head l))
                   else acc


multiples_of_3_or_5 = [x | x <- [1..999], is_multiple_of_3_or_5 x]

solution = reduce_sum multiples_of_3_or_5 0
