module Test.Prover.Tactics where

import Control.Monad.State
import Control.Lens
import Data.Generics.Product.Fields
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Language.Syntax
import Prover.Types
import Prover.Tactics

tests :: TestTree
tests = testGroup "Tests"
    [ testProperty "intros adds premise to environment"
        prop_introsAddPremise
    ]

prop_introsAddPremise :: Name -> Property
prop_introsAddPremise asName =
    forAll (arbitrary `suchThat` (\st -> not . null $ st ^. field @"premises"))
        $ \st ->
            let st' = execState (intros asName) st
                pre = head $ st ^. field @"premises"
            in (asName, pre) `elem` (st' ^. field @"env")
