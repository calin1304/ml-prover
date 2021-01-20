module Prover.Types where

import           Language.Syntax

type Premises = [Expr]
type Goal = Expr
type Tactics = [Tactic]
type Name = String
