myListLength :: [a] -> Int

myListLength [] = 0
myListLength (_:xs) = 1 + (myListLength xs)