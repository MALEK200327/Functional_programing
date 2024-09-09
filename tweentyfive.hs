import Data.Foldable (Foldable(fold))
factors n = [x | x <- [1..n `div` 2], n `mod` x == 0]



