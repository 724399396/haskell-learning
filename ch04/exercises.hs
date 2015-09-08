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
