luhnDouble :: Int -> Int
luhnDouble n
    | x > 9     = x - 9
    | otherwise = x
  where x = 2 * n

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d =
    ((luhnDouble a) + b + (luhnDouble c) + d) `mod` 10 == 0

