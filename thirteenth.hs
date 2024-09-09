
satisfies check n 
 | check n = "the number" ++ show n ++ "passes check"
 | otherwise = "the number" ++ show n  ++ " doesn't pass the check"


f x xs = length [ a | a <- xs, a == x ]


f1 xs = [(y,f2 y)| y<-xs]
  where
    f2 y = length [ a | a <- xs, a == y ]



fn _ [] = []
fn [] _ =[]

fn (a:as) (b:bs)
  |a<b = (a,b) : fn as bs
  |otherwise = fn (a:as) bs