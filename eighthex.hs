roots (a, b, c)
    | discriminant > 0 = (x1, x2)
    | discriminant == 0 = (x1, x1)
    | otherwise = error "No real roots"
  where
    discriminant = b * b - 4 * a * c
    x1 = (-b + sqrt discriminant) / (2 * a)
    x2 = (-b - sqrt discriminant) / (2 * a)
