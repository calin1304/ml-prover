module Utils where

import           Data.Maybe (fromJust)

unsafeSplitHead :: [a] -> (a, [a])
unsafeSplitHead (x:xs) = (x, xs)

unsafeLookup :: Eq k => k -> [(k, v)] -> v
unsafeLookup k = fromJust . lookup k

allA :: (Applicative f, Traversable t) => (a -> f Bool) -> t a -> f Bool
allA p = fmap and . traverse p
