--file: ch03/BogusPattern.hs
data Fruit = Apple | Orange
            deriving(Show)

apple = "apple"

orange = "orange"

whichFruit :: String -> Fruit

whichFruit f = case f of
                apple -> Apple
                orange -> Orange

betterFruit f = case f of
                 "apple" -> Apple
                 "orange" -> Orange
