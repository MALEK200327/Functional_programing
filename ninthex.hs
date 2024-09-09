getOddItems list = filter isOdd list
  where
    isOdd x = x `mod` 2 /= 0