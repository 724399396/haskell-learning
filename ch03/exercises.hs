import Data.List

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

mean :: [Float] -> Float
mean = flip (/).fromIntegral.length <*> sum

palindrom :: [a] -> [a]
palindrom = (++) <*> reverse

isPalindrom :: (Eq a) => [a] -> Bool
isPalindrom = (==) <*> reverse

sortSubL :: [[a]] -> [[a]]
sortSubL = sortBy (\x y -> length x `compare` length y)

myIntersperse :: a -> [[a]] -> [a]
myIntersperse s = init . foldr (\x acc -> x ++ (s:acc)) []

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving(Show)

treeHeight :: Tree a -> Int
treeHeight Empty = 0
treeHeight (Node _ l r) = 1 + max (treeHeight l) (treeHeight r)


data Point = Point Int Int
data Direction = Direction Point Point Point

direct [a,b,c] = Direction a b c

directs :: [Point] -> [Direction]
directs = map direct . filter ((==3) . length) . map (take 3) . tails
