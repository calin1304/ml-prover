module Utils where

unsafeSplitHead :: [a] -> (a, [a])
unsafeSplitHead (x:xs) = (x, xs)
