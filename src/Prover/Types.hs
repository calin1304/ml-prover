module Prover.Types where

import Language.Syntax

type Premises = [SimpleExpr]
type Goal = SimpleExpr
type ProofEnv = [(String, SimpleExpr)]
type Tactics = [Tactic]
type Name = String