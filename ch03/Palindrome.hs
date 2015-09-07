reverseList [] = []
reverseList (x:xs) = (reverseList xs) ++ [x]

palindrome xs = xs ++ (reverseList xs)

isPalindrome [] = True
isPalindrome (x:xs) | odd (length xs) && (length xs) == 1 = x == (last xs)
					| odd (length xs) = x == (last xs) && isPalindrome (init xs)
					| otherwise = False