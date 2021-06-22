module Test.Utils where

import           Control.Lens          (at, (^.))
import           Data.Either           (isLeft)
import qualified Data.Map.Strict       as M (fromList, singleton)
import           Data.Maybe            (isJust)
import           Test.Tasty            (TestTree, testGroup)
import           Test.Tasty.HUnit      (assertBool, assertEqual, assertFailure,
                                        testCase, (@?=))
import           Test.Tasty.QuickCheck ()

import           Interp                (isTop)
import           Language.Lexer        ()
import           Language.Parser       ()
import           Language.ParserM      ()
import           Language.Syntax       (Declaration (Rule), Expr, Tactic, ( # ))
import           Prover.ProofM         (Context, ProofM, ProofState,
                                        ProverError, emptyProofState,
                                        mkProofState, newName, runProofM,
                                        _context, _goal)
import           Prover.Tactics
import           Prover.Types          (Goal)

infix 5 .->
(.->) :: Expr -> Expr -> Expr
(.->) p q = "impl" # p # q

infix 4 |-
(|-) :: [(String, Declaration)] -> Goal -> ProofM () -> Either ProverError ((), ProofState)
(|-) c g p = runProofM p (mkProofState g (M.fromList c))

infix 4 .|-
(.|-) :: [Expr] -> Expr -> Declaration
(.|-) = Rule []
