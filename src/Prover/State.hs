module Prover.State where

import GHC.Generics

import Language.Syntax
import Prover.Types

data ProofState = ProofState
    { goal :: Goal
    , premises :: Premises
    , env :: ProofEnv
    }
    deriving (Show, Generic)

emptyProofEnv :: ProofEnv
emptyProofEnv = []