module Prover.ProofM where

import           Control.Arrow                ((&&&))
import           Control.Lens                 (modifying)
import           Control.Monad.State
import           Data.Generics.Product.Fields (field)
import           Data.Maybe                   (fromMaybe)
import           GHC.Generics                 (Generic)
import           Test.QuickCheck              (Arbitrary, arbitrary)
import           Text.Printf                  (printf)

import           Language.Syntax
import           Prover.Substitution
import           Prover.Types
import           Utils

type ProofM a = State ProofState a

type ProofEnv = [(String, Declaration)]

data ProofState = ProofState
    { goal     :: Goal
    , premises :: Premises
    , env      :: ProofEnv
    }
    deriving (Eq, Show, Generic)

runProofM :: ProofM a -> ProofState -> (a, ProofState)
runProofM = runState

step :: Tactic -> ProofM ()
step = \case
    Specialize expr asName -> specialize expr asName

specialize :: Expr -> String -> ProofM ()
specialize expr asName =
    case expr of
        Application sym args -> do
            (sdef, sargs) <- (getDefinition &&& getArgs) <$> lookupSymbol sym
            let s = mkSubst sargs args
            addToEnv (applySubst s sdef) asName
        _ -> error "Specializing something which is not an application"

addToEnv :: Expr -> String -> ProofM ()
addToEnv expr asName =
    modifying (field @"env") ((asName, Rule asName [] expr):)

lookupSymbol :: String -> ProofM Declaration
lookupSymbol name =
    fromMaybe (error $ printf "Symbol %s not found\n." name)
        <$> gets (lookup name . env)
