module Prover.ProofM where

import           Control.Arrow                ((&&&))
import           Control.Lens                 (modifying)
import           Control.Monad.State
import           Data.Function                ((&))
import           Data.Generics.Product.Fields (field)
import           Data.List                    (uncons)
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
    Intros asName -> intros asName
    Specialize expr asName -> specialize expr asName
    -- Apply expr asName -> apply expr asName
    -- Exact name -> exact name

-------------
-- Tactics --
-------------

-- FIXME: Doesn't work with implication
intros :: Name -> ProofM ()
intros asName =
    modify $ \st ->
        let (pre, rest) =
                uncons (premises st)
                    & fromMaybe (error "Trying to intros with no premises")
            env' = (asName, Rule asName [] pre) : env st
         in st { env = env', premises = rest }

specialize :: Expr -> String -> ProofM ()
specialize expr asName =
    case expr of
        Application sym args -> do
            (sdef, sargs) <- (getDefinition &&& getArgs) <$> lookupSymbol sym
            let s = mkSubst sargs args
            addToEnv (applySubst s sdef) asName
        _ -> error "Specializing something which is not an application"

apply :: Expr -> String -> ProofM ()
apply = undefined

exact :: String -> ProofM ()
exact = undefined



-------------
-- Helpers --
-------------

addToEnv :: Expr -> String -> ProofM ()
addToEnv expr asName =
    modifying (field @"env") ((asName, Rule asName [] expr):)

lookupSymbol :: String -> ProofM Declaration
lookupSymbol name =
    fromMaybe (error $ printf "Symbol %s not found\n." name)
        <$> gets (lookup name . env)
