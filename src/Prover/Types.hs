module Prover.Types where

import Parser.Syntax

type Premises = [SimpleExpr]
type Goal = SimpleExpr
type ProofEnv = [(String, SimpleExpr)]
type Tactics = [Tactic]