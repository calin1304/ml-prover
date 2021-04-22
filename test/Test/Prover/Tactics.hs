module Test.Prover.Tactics where

import Control.Monad.State
import Control.Lens
import Data.Generics.Product.Fields
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Control.Monad.State (execState)
import Prover.ProofM (ProofState (..))

import Language.Syntax
import Prover.ProofM

tests :: TestTree
tests = testGroup "Tactics"
    [ -- testProperty "intros adds premise to environment"
        --prop_introsAddPremise
      testCase
        "specialize adds a new rule instantiated from an existing rule"
        test_specialize
    , testCase
        "specialize works with modus ponens"
        test_specialize_mp
    , testCase
        "exact tactic works as expected"
        test_exact
    ]

-- prop_introsAddPremise :: Name -> Property
-- prop_introsAddPremise asName =
--     forAll (arbitrary `suchThat` (\st -> not . null $ st ^. field @"premises"))
--         $ \st ->
--             let st' = execState (intros asName) st
--                 pre = head $ st ^. field @"premises"
--             in (asName, pre) `elem` (st' ^. field @"env")

test_specialize = actual @?= expected
  where
    expected = Rule "r" [] $ FromDerive [] (Ident "X")
    actual = evalState (specialize' sp) pst

    sp = Ident "r" ## Ident "X"
    r = Rule "r" ["P"] $ FromDerive [] (Ident "P")
    pst = ProofState (Ident "") [] [("r", r)]

test_specialize_mp = actual @?= expected
  where
    expected = Rule "mp" [] $ FromDerive [Ident "X", Ident "X" ## Ident "Y"] (Ident "Y")
    actual = evalState (specialize' sp) pst

    sp = Ident "mp" ## Ident "X" ## Ident "Y"
    pst = ProofState (Ident "") [] [("mp", mp)]
    mp = Rule "mp" ["P", "Q"] $ FromDerive [Ident "P", Ident "P" ## Ident "Q"] (Ident "Q")

test_exact = actual @?= expected
  where
    expected = True
    actual = evalState (exact "a") pst
    pst = ProofState (Ident "P") [] [("a", a)]
    a = Rule "a" ["P"] (FromDerive [] (Ident "P"))
