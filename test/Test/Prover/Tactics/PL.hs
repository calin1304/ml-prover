module Test.Prover.Tactics.PL
    ( tests
    ) where

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
import           Test.Utils

tests :: TestTree
tests =
    testGroup "Propositional Logic proofs"
        [ testCase "simple proof" proofTest
        , testCase "mp2" proof_mp2
        , testCase "mpd" proof_mpd
        ]
  where
    proofTest = do
        let proof = do
                specialize ("mp" # "X") "H1"
                specialize ("H1" # "Y") "H2"
                -- apply "Hss"
                --     [ [ exact "X"]
                --     , [ exact "H"]
                --     ]
                assumptions "H2" "H3"
                exact "H3"
        let Right (_, st) =
                [ ("mp", Rule ["P", "Q"] ["P", "P" .-> "Q"] "Q")
                , ("X", [] .|- "X")
                , ("Y", [] .|- "Y")
                , ("H", [] .|- ("X" .-> "Y"))
                ]
                |- "Y" $ proof
        assertBool "goal is satisfied" $ isTop $ st ^. _goal

    proof_mp2 = do
        let proof = do
                apply "X1"
                    [ apply "X"  []
                    , apply "X0" []
                    ]
        let Right (_, st) =
                [ ("X", [] .|- "P")
                , ("X0", [] .|- "Q")
                , ("X1", ["P", "Q"] .|- "R")
                ]
                |- "R" $ proof
        assertEqual "goal is top" "top" (st ^. _goal)

    proof_mpd = do
        let proof = do
                apply "X0"
                    [ apply "X1" []
                    , apply "X"
                        [ apply "X1" [] -- FIXME: This should fail but it doesn't
                        ]
                    ]
        let Right (_, st) =
                [ ("X", ["P"] .|- "Q")
                , ("X0", ["P", "Q"] .|- "R")
                , ("X1", [] .|- "P")
                ]
                |- "R" $ proof
        assertEqual "goal is top" "top" (st ^. _goal)
