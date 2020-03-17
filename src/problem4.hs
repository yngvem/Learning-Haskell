is_palindrome :: (Eq a) => [a] -> Bool
is_palindrome [] = True
is_palindrome (l:ls)
    | ls == []       = True
    | otherwise      = (l == (last ls)) && is_palindrome (init ls)

three_digit_numbers = [999, 998..100]
three_digit_products = [x*y | x <- three_digit_numbers, y <- three_digit_numbers]

solution = foldl1 max $ filter is_palindrome_number three_digit_products
           where is_palindrome_number = is_palindrome . show
