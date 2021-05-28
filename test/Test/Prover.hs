module Test.Prover
    ( tests
    ) where

import           Control.Lens          (at, (^.))
import           Data.Either           (isLeft)
import qualified Data.Map.Strict       as M (fromList)
import           Data.Maybe            (isJust)
import           Test.Tasty            (TestTree, testGroup)
import           Test.Tasty.HUnit      (assertBool, assertEqual, assertFailure,
                                        testCase, (@?=))
import           Test.Tasty.QuickCheck ()

import           Interp                (isTop)
import           Language.Lexer        ()
import           Language.Parser       ()
import           Language.ParserM      ()
import           Language.Syntax       (Declaration (Rule), axiom, ( ## ))
import           Prover.ProofM         (ProofState (ProofState), apply,
                                        assumptions, env, exact, goal,
                                        runProofM, specialize, _env, _goal)

tests :: TestTree
tests = testGroup "Prover"
    [ specializeTacticTests
    , exactTacticTests
    , applyTacticTests
    , proofTests
    ]

specializeTacticTests :: TestTree
specializeTacticTests =
    testGroup "specialize tactic"
        [ testCase "partial specialization" partialSpecialization
        , testCase "total specialization" totalSpecialization
        , testCase "application expression" applicatioSpecialization
        ]
  where
    -- TODO: Test that it fails if rule to specialize is not in the context
    partialSpecialization = do
        let Right (_, st) =
                runProofM
                    (specialize ("mp" ## "X") "Hs")
                    (ProofState
                        { goal = "Y"
                        , env =
                            M.fromList
                                [ ("mp", Rule "mp" ["P", "Q"] ["P", "impl" ## "P" ## "Q"] "Q")
                                , ("X", axiom "X" "X")
                                ]
                        }
                    )
        assertBool "specialize result is in environment" $ isJust $ st ^. _env . at "Hs"
        let Just (Rule name args hs c) = st ^. _env . at "Hs"
        name @?= "Hs"
        args @?= ["Q"]
        hs @?= ["X", "impl" ## "X" ## "Q"]
        c @?= "Q"

        -- TODO: Add test that tries to specialize with something that has hypotheses

    totalSpecialization = do
        let Right (_, st) =
                runProofM
                    (specialize ("Hs" ## "Y") "Hss")
                    (ProofState
                        { goal = "Y"
                        , env =
                            M.fromList
                                [ ("mp", Rule "mp" ["P", "Q"] ["P", "impl" ## "P" ## "Q"] "Q")
                                , ("X", axiom "X" "X")
                                , ("Y", axiom "Y" "Y")
                                , ("Hs", Rule "Hs" ["Q"] ["X", "impl" ## "X" ## "Q"] "Q")
                                ]
                        }
                    )
        assertBool "specialize result is in environment" $ isJust $ st ^. _env . at "Hss"
        let Just (Rule name args hs c) = st ^. _env . at "Hss"
        name @?= "Hss"
        args @?= []
        hs @?= ["X", "impl" ## "X" ## "Y"]
        c @?= "Y"

    applicatioSpecialization = do
        let Right (_, st) =
                runProofM
                    (specialize ("r" ## "H") "Hss")
                    (ProofState
                        { goal = "Y"
                        , env =
                            M.fromList
                                [ ("r", Rule "r" ["P"] ["impl" ## "P" ## "P"] "P")
                                , ("H", axiom "H" ("impl" ## "X" ## "Y"))
                                ]
                        }
                    )
        assertBool "specialize result is in environment" $ isJust $ st ^. _env . at "Hss"
        let Just (Rule name args hs c) = st ^. _env . at "Hss"
        name @?= "Hss"
        args @?= []
        hs @?= ["impl" ## ("impl" ## "X" ## "Y") ## ("impl" ## "X" ## "Y")]
        c @?= "impl" ## "X" ## "Y"

exactTacticTests :: TestTree
exactTacticTests =
    testGroup "exact tactic"
        [ testCase "works as expected" exactTest
        ]
  where
    exactTest = do
        let Right (_, st) =
                runProofM
                    (exact "H")
                    (ProofState
                        { goal = "Y"
                        , env = M.fromList [("H", axiom "H" "Y")]
                        }
                    )
        assertBool "goal is satisfied" $ isTop $ st ^. _goal

applyTacticTests :: TestTree
applyTacticTests =
    testGroup "apply tactic"
        [ testCase "works as exact if there are no hypotheses" noHypotheses
        , testCase "fails if goal doesn't match conclusion" noMatch
        ]
  where
    noHypotheses = do
        let result =
                runProofM
                    (apply "H" [])
                    (ProofState
                        { goal = "P"
                        , env = M.fromList [("H", Rule "H" [] [] "P")]
                        }
                    )
        case result of
            Left e -> assertFailure e
            Right (_, st) -> assertBool "goal is satisfied" $ isTop $ st ^. _goal

    noMatch = do
        let result =
                runProofM
                    (apply "H" [])
                    (ProofState
                        { goal = "P"
                        , env = M.fromList [("H", Rule "H" [] [] "Q")]
                        }
                    )
        assertBool "fail because goal doesn't match conclusion of applied formula"
            $ isLeft result

proofTests :: TestTree
proofTests =
    testGroup "proofs"
        [ testCase "simple proof" proofTest
        , testCase "mp2" proof_mp2
        , testCase "mpd" proof_mpd
        ]
  where
    proofTest = do
        let proof = do
                specialize ("mp" ## "X") "H1"
                specialize ("H1" ## "Y") "H2"
                -- apply "Hss"
                --     [ [ exact "X"]
                --     , [ exact "H"]
                --     ]
                assumptions "H2" "H3"
                exact "H3"
        let Right (_, st) =
                runProofM
                    proof
                    (ProofState
                        { goal = "Y"
                        , env =
                            M.fromList
                                [ ("mp", Rule "mp" ["P", "Q"] ["P", "impl" ## "P" ## "Q"] "Q")
                                , ("X", axiom "X" "X")
                                , ("Y", axiom "Y" "Y")
                                , ("H", axiom "H" ("impl" ## "X" ## "Y"))
                                ]
                        }
                    )
        assertBool "goal is satisfied" $ isTop $ st ^. _goal

    proof_mp2 = do
        let proof = do
                apply "X1"
                    [ apply "X"  []
                    , apply "X0" []
                    ]
        let Right (_, st) =
                runProofM
                    proof
                    (ProofState
                        { goal = "R"
                        , env =
                            M.fromList
                                [ ("X", axiom "X" "P")
                                , ("X0", axiom "X0" "Q")
                                , ("X1", Rule "X1" [] ["P", "Q"] "R")
                                ]
                        }
                    )
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
                runProofM
                    proof
                    (ProofState
                        { goal = "R"
                        , env =
                            M.fromList
                                [ ("X", Rule "X" [] ["P"] "Q")
                                , ("X0", Rule "X0" [] ["P", "Q"] "R")
                                , ("X1", Rule "X1" [] [] "P")
                                ]
                        }
                    )
        assertEqual "goal is top" "top" (st ^. _goal)
