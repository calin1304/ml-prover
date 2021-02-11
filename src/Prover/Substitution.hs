module Prover.Substitution where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromMaybe)

import           Language.Syntax

newtype Subst = Subst (Map String Expr)
                deriving (Show)

-- TODO: Should I change to (Foldable f => f (String, Expr)) ?
mkSubst :: [String] -> [Expr] -> Subst
mkSubst ns es = Subst . M.fromList $ zip ns es

applySubst :: Subst -> Expr -> Expr
applySubst (Subst s) = undefined -- \case
    -- Ident name -> fromMaybe (Ident name) $ M.lookup name s
    -- Application sym args -> Application sym (map (applySubst (Subst s)) args)
    -- FromDerive ps es -> FromDerive ps (applySubst (Subst s) es)
