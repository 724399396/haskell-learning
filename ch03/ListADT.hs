-- file: ch03/ListADT.hs
data List a = Cons a (List a)
              | Nil
                deriving (Show)

fromList (x:xs) = Cons x (fromList xs)
fromList [] = Nil

fromCons (Cons x xs) = x : (fromCons xs)
fromCons Nil = []
