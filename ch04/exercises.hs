import Data.Char (digitToInt,isDigit,isSpace)
import Data.List

safeHead :: [a] -> Maybe a
safeTail :: [a] -> Maybe [a]
safeLast :: [a] -> Maybe a
safeInit :: [a] -> Maybe [a]

safeHead list = case list of
                [] -> Nothing
                (x:_) -> Just x
safeTail list = case list of
                [] -> Nothing
                (_:xs) -> Just xs
safeLast list = case list of
                [] -> Nothing
                (x:[]) -> Just x
                (_:xs) -> safeLast xs
safeInit list = if null list
                then Nothing
                else Just (init list)

splitWith :: (a -> Bool) -> [a] -> [[a]]

splitWith predicate [] = []
splitWith predicate xs = let (pre,post) = break predicate xs
                             (_,left) = span predicate post
                         in  pre : splitWith predicate left

concateListWithNewLine (x:xs) = x ++ "\n" ++ concateListWithNewLine xs
concateListWithNewLine [] = []

concateChar2CharList x y = x : y : []

transpose x = let [part1,part2] = words x
              in concateListWithNewLine(zipWith concateChar2CharList part1 part2)

asInt_fold　:: String -> Int
asInt_fold ('-':xs) = negate (asInt_fold xs)
asInt_fold xs = foldl step 0 xs
                             where step acc x = acc * 10 + digitToInt x

type ErrorMessage = String
asInt_either :: String -> Either ErrorMessage Int
asInt_either ('-':xs) | all isDigit xs = Right $ asInt_fold ('-':xs)
asInt_either xs | all isDigit xs = Right $ asInt_fold xs
asInt_either xs = Left $ "non-digit '" ++ [head $ filter (not.isDigit) xs] ++ "'"

myConcat :: [[a]] -> [a]
myConcat xs = foldr (++) [] xs

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile _ [] = []
myTakeWhile pre (x:xs) | pre x = x : myTakeWhile pre xs
                       | otherwise = []

myTakeWhile2 :: (a -> Bool) -> [a] -> [a]
myTakeWhile2 pre xs = foldr step [] xs
    where step a b | pre a = a : b
                   | otherwise = b

myGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
myGroupBy pred xs = let (nowSame,acc) =  foldr step ([],[]) xs
                    in
                      nowSame:acc
                      where step x (nowSame,acc) = if (null nowSame || pred x (head nowSame))
                               then (x:nowSame, acc)
                               else ([x], nowSame:acc)

myAny pre xs = foldl' step False xs
    where step init x = pre x || init

myWords xs = let (word,acc) = foldr step ([],[]) xs
             in filter (not.null) (word:acc)
               where step x (unCompleteWord,acc) = if (isSpace x)
                                      then ([],unCompleteWord:acc)
                                      else (x:unCompleteWord,acc)

myUnlines :: [String] -> String
myUnlines = foldr (\x acc -> x ++ "\n" ++ acc) ""
