module Prover.State where

import Language.Syntax
import Prover.Types

data ProofState = ProofState
    { goal :: Goal
    , premises :: Premises
    , env :: ProofEnv
    }
    deriving (Show)

emptyProofEnv :: ProofEnv
emptyProofEnv = []