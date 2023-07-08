module Main
    ( main
    ) where

import           Test.Tasty          (defaultMain, testGroup)

import qualified Test.Prover.Tactics as Prover.Tactics (tests)
import qualified Test.Language.Parser as Language.Parser (tests)
import qualified Test.Language.Lexer as Language.Lexer (tests)

main :: IO ()
main =
    defaultMain
        $ testGroup "Tests"
            [ Language.Lexer.tests
            , Language.Parser.tests
            , Prover.Tactics.tests
            ]
