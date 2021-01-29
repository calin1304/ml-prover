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
import Prover.Tactics

tests :: TestTree
tests = testGroup "Tests"
    [ -- testProperty "intros adds premise to environment"
        --prop_introsAddPremise
    testCase "specialize tactic" test_specialize
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
    sp = Specialize (Application "a1" [EVar "X", EVar "Y"]) "H1"
    pst = ProofState (EVar "") [] [("a1", r)]
    r = Rule "a1" ["P", "Q"] (FromDerive [] (Application "impl" [EVar "P", Application "impl" [EVar "Q", EVar "P"]]))
    expected =
        let ar = Rule "H1" [] (Application "impl" [EVar "X", Application "impl" [EVar "Y", EVar "X"]])
         in ProofState (EVar "") [] [("H1", ar), ("a1", r)]
    actual = execState (specialize sp) pst