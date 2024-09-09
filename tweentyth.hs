
euclid a 0 = a  -- If the second number is 0, the GCD is the first number.
euclid a b = euclid b (a `mod` b)  -- Recursive step, swapping a and b as needed.