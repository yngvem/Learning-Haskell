pythagorean_triplets = 
    let isInt x = x == fromInteger (round x)
    in [[a, b, c] | a <- [1..1000], b <- [a..1000], let c = sqrt (a^2 + b^2),
                    isInt c, a + b + c == 1000]

solution = round . product . head $ pythagorean_triplets
