module Prover.ProofM where

import           Control.Monad.State
import           GHC.Generics        (Generic)
import           Test.QuickCheck     (Arbitrary, arbitrary)

import           Prover.Types

type ProofM a = State ProofState a

data ProofState = ProofState
    { goal     :: Goal
    , premises :: Premises
    , env      :: ProofEnv
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

runProof :: Tactics -> Premises -> Goal -> Bool
runProof tactics premises goal =
    evalState
        (prove tactics)
        (ProofState goal premises emptyProofEnv)

prove :: Tactics -> ProofM Bool
prove = undefined
