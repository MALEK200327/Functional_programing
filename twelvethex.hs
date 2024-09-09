luhnDouble x
    | doubled > 9 = doubled - 9
    | otherwise = doubled
    where doubled = x * 2

luhn a b c d = total `mod` 10 == 0
    where total = luhnDouble a+b+c+d