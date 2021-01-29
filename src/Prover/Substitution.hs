module Prover.Substitution where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromMaybe)

import           Language.Syntax

newtype Subst = Subst (Map String Expr)
                deriving (Show)

mkSubst :: [String] -> [Expr] -> Subst
mkSubst ns es = Subst . M.fromList $ zip ns es

applySubst :: Subst -> Expr -> Expr
applySubst (Subst s) = \case
    EVar name -> fromMaybe (EVar name) $ M.lookup name s
    SVar name -> fromMaybe (SVar name) $ M.lookup name s
    Application sym args -> Application sym (map (applySubst (Subst s)) args)
    FromDerive ps es -> FromDerive ps (applySubst (Subst s) es)
