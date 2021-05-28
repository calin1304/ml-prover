module Main
    ( main
    ) where

import           Test.Tasty  (defaultMain, testGroup)

import qualified Test.Prover as Prover (tests)

main :: IO ()
main =
    defaultMain
        $ testGroup "Tests"
            [ Prover.tests
            ]
