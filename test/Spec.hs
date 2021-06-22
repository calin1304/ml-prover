module Main
    ( main
    ) where

import           Test.Tasty          (defaultMain, testGroup)

import qualified Test.Prover.Tactics as Prover.Tactics (tests)

main :: IO ()
main =
    defaultMain
        $ testGroup "Tests"
            [ Prover.Tactics.tests
            ]
