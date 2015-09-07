--file ch03/Intersperse.hs
intersperse :: a -> [[a]] -> [a]

intersperse sperate [] = []
intersperse sperate	(x:xs) | null xs = x
						   | otherwise = ((x ++ [sperate]) ++ (intersperse sperate xs))