module Prover.State where

import GHC.Generics
import Test.QuickCheck

import Language.Syntax
import Prover.Types

data ProofState = ProofState
    { goal :: Goal
    , premises :: Premises
    , env :: ProofEnv
    }
    deriving (Show, Generic)

instance Arbitrary ProofState where
    arbitrary =
        ProofState
            <$> arbitrary
            <*> arbitrary
            <*> arbitrary

emptyProofEnv :: ProofEnv
emptyProofEnv = []
