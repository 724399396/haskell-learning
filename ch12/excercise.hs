takeTuple :: Int -> (a,a,a,a) -> a
takeTuple 3 (_,_,_,x) = x
takeTuple 2 (_,_,x,_) = x
takeTuple 1 (_,x,_,_) = x
takeTuple 0 (x,_,_,_) = x


takeTupleSix :: Int -> (a,a,a,a,a,a) -> a
takeTupleSix 5 (_,_,_,_,_,x) = x
takeTupleSix 4 (_,_,_,_,x,_) = x
takeTupleSix 3 (_,_,_,x,_,_) = x
takeTupleSix 2 (_,_,x,_,_,_) = x
takeTupleSix 1 (_,x,_,_,_,_) = x
takeTupleSix 0 (x,_,_,_,_,_) = x
