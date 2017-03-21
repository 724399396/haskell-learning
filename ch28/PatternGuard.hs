{-# LANGUAGE PatternGuards #-}

testme x xs | Just y <- lookup x xs, y > 3 = y
            | otherwise                    = 0

testme_noguards x xs = case lookup x xs of
                         Just y | y > 3 -> y
                         _              -> 0
