module Prover.Substitution where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromMaybe)

import           Language.Syntax

newtype Subst = Subst (Map String Expr)
                deriving (Show)

-- TODO: Should I change to (Foldable f => f (String, Expr)) ?
mkSubst :: [(String, Expr)] -> Subst
mkSubst = Subst . M.fromList

applySubst :: Subst -> Expr -> Expr
applySubst subst@(Subst s) = \case
    Ident name -> fromMaybe (Ident name) $ M.lookup name s
    Application e1 e2 -> Application (applySubst subst e1) (applySubst subst e2)
