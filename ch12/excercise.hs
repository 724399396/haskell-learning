takeTuple :: Int -> (a,a,a,a) -> a
takeTuple 4 (_,_,_,x) = x
takeTuple 3 (_,_,x,_) = x
takeTuple 2 (_,x,_,_) = x
takeTuple _ (x,_,_,_) = x
