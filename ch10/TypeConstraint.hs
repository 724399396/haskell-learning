data (Ord a) => OrdStack a = Bottom
                            | Item a (OrdStatck a)
                              deriving (Show)

isIncreasing :: (Ord a) => OrdStack a -> Bool
isIncreasing (Item a rest@(Item b _))
   | a < b = isIncreasing rest
   | otherwise = False
isIncreasing _ = True

push :: (Ord a) => a -> OrdStack a -> OrderStack a
push a s = Item a s
