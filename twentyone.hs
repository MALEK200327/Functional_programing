import GHC.OldList (findIndex)
summ [] = 0
summ (x:xs) = x + summ(xs)  

fn []=[]
fn (x:xs) = x:[k|k<-fn xs ,k/=x]



f=(+)


f1 xs =[(x,f2 x)|x<-xs]
  where
  f2 x = length [a|a<-xs,a==x]
