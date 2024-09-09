
safeTail xs = if null xs then [] else tail xs


safeTail2 xs
    | null xs   = []
    | otherwise = tail xs
