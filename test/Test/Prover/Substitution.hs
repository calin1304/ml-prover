module Test.Prover.Substitution where

import Control.Monad.State
import Control.Lens
import Data.Generics.Product.Fields
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Control.Monad.State (execState)
import Prover.ProofM (ProofState (..))

import Language.Syntax
import Prover.Substitution

tests :: TestTree
tests = testGroup "Substitution"
    [ testCase
        "substitution works with identifiers"
        test_substitution_ident
    , testCase
        "substitution works with application"
        test_substitution_application
    ]

test_substitution_ident = actual @?= expected
  where
    expected = Ident "X"
    actual = applySubst (mkSubst [("P", Ident "X")]) (Ident "P")

test_substitution_application = actual @?= expected
  where
    expected = Ident "a" ## (Ident "b" ## Ident "X" ## Ident "Y") ## Ident "Z"
    actual =
        applySubst
            ( mkSubst
                [ ("P", Ident "b" ## Ident "X" ## Ident "Y")
                , ("Q", Ident "Z")
                ]
            )
            (Ident "a" ## Ident "P" ## Ident "Q")