data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving(Show)

simpleTree = Node "parent" (Node "left child" Empty Empty)
                           (Node "right child" Empty Empty)

height t = case t of
                (Node a left right) -> 1 + ( if (height left) > (height right) then (height left) else (height right))
                Empty -> 0