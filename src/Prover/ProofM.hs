module Prover.ProofM where

import           Control.Lens                 (modifying)
import           Control.Monad.State
import           Data.Generics.Product.Fields (field)
import           GHC.Generics                 (Generic)
import           Test.QuickCheck              (Arbitrary, arbitrary)

import           Language.Syntax
import           Prover.Types

type ProofM a = State ProofState a

type ProofEnv = [(String, Declaration)]

data ProofState = ProofState
    { goal     :: Goal
    , premises :: Premises
    , env      :: ProofEnv
    }
    deriving (Eq, Show, Generic)

-- instance Arbitrary ProofState where
--     arbitrary = ProofState <$> arbitrary <*> arbitrary <*> arbitrary

evalModule :: ModDef -> ProofM ()
evalModule (ModDef _ (decl:decls)) = 
    case decl of
        x@(Rule name _ _) -> addToEnv (name, x)
        x@(Lemma name _ _ _) -> addToEnv (name, x)
        _ -> undefined

addToEnv :: (String, Declaration) -> ProofM ()
addToEnv (name, expr) = modifying (field @"env") ((name, expr):)
