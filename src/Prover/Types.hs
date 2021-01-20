module Prover.Types where

import           Language.Syntax

type Premises = [Expr]
type Goal = Expr
type ProofEnv = [(String, Expr)]
type Tactics = [Tactic]
type Name = String
