module Utils where

import           Data.Maybe  (fromJust)
import           Debug.Trace
import           Text.Printf (printf)

allA :: (Applicative f, Traversable t) => (a -> f Bool) -> t a -> f Bool
allA p = fmap and . traverse p

showSection :: Show a => String -> a -> IO ()
showSection title a = printf "\n%s\n---\n%s\n---\n" title (show a)

debugSection :: Show a => String -> a -> ()
debugSection title a =
    trace (printf "\n%s\n---\n%s\n---\n" title (show a)) ()
