module Prover.Types
    ( Premises
    , Goal
    , Tactics
    , Name
    ) where

import           Language.Syntax (Expr, Tactic)

type Premises = [Expr]
type Goal = Expr
type Tactics = [Tactic]
type Name = String
