module Test.Prover
    ( tests
    ) where

import           Control.Lens
import qualified Data.Map.Strict       as M (fromList)
import           Data.Maybe            (isJust)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import           Interp                (isTop)
import           Language.Lexer
import           Language.Parser
import           Language.ParserM
import           Language.Syntax
import           Prover.ProofM

tests :: TestTree
tests = testGroup "Prover"
    [ specializeTacticTests
    , exactTacticTests
    , proofTests
    ]

test_prop_logic = undefined -- actual @?= expected
--   where
--     expected = True
--     actual = source >>= checkFile

--     source :: IO String
--     source = readFile "test/data/test_prop_logic.mlp"

--     checkFile :: FilePath -> IO (Either e ())
--     checkFile = parseFile >>= runInterpM

{-
specialize tests
env: H1 : X, H2 : X -> Y
goal: Y

specialize (mp H1) as H3
env: H1 : X, H2 : X -> Y, H3 : mp H1

specialize (mp H1 H2) as H3
env: H1 : X, H2 : X -> Y : H3 : Y
-}

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
        let Right (a, st) =
                runProofM
                    (specialize ("mp" ## "X") "Hs")
                    (ProofState
                        { goal = "Y"
                        , premises = []
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
        let Right (a, st) =
                runProofM
                    (specialize ("Hs" ## "Y") "Hss")
                    (ProofState
                        { goal = "Y"
                        , premises = []
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
        let Right (a, st) =
                runProofM
                    (specialize ("r" ## "H") "Hss")
                    (ProofState
                        { goal = "Y"
                        , premises = []
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
        let Right (a, st) =
                runProofM
                    (exact "H")
                    (ProofState
                        { goal = "Y"
                        , premises = []
                        , env = M.fromList [("H", axiom "H" "Y")]
                        }
                    )
        assertBool "goal is satisfied" $ isTop $ st ^. _goal

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
        let Right (a, st) =
                runProofM
                    proof
                    (ProofState
                        { goal = "Y"
                        , premises = []
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
        let Right (a, st) =
                runProofM
                    proof
                    (ProofState
                        { goal = "R"
                        , premises = []
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
                        [ apply "X" [] -- FIXME: This should fail but it doesn't
                        ]
                    ]
        let Right (a, st) =
                runProofM
                    proof
                    (ProofState
                        { goal = "R"
                        , premises = []
                        , env =
                            M.fromList
                                [ ("X", Rule "X" [] ["P"] "Q")
                                , ("X0", Rule "X0" [] ["P", "Q"] "R")
                                , ("X1", Rule "X1" [] [] "P")
                                ]
                        }
                    )
        print st
        assertEqual "goal is top" "top" (st ^. _goal)
