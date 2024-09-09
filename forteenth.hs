increment = \x -> x + 1
decrement = \x -> x - 1

isPrime n
    | n <= 1    = False   -- 0 and 1 are not prime
    | otherwise = all (\x -> n `mod` x /= 0) [2..floor (sqrt (fromIntegral n))]