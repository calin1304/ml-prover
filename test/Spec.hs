module Main(main) where

import Test.Tasty

import qualified Test.Language.Parser as Parser (tests)
import qualified Test.Prover.Tactics as Tactics (tests)
import qualified Test.Prover.Substitution as Substitution (tests)

main =
    defaultMain
        $ testGroup "Tests"
            [ Parser.tests
            , Tactics.tests
            , Substitution.tests
            ]
