module Test.Prover
    ( tests
    ) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Data.Map.Strict as M (fromList)
import Control.Lens
import Data.Maybe (isJust)

import Language.Syntax
import Language.Lexer
import Language.Parser
import Language.ParserM
import Prover.ProofM

tests :: TestTree
tests = testGroup "Prover"
    [ specializeTacticTest
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

specializeTacticTest :: TestTree
specializeTacticTest = 
    testGroup "specialize tactic"
        [ testCase "partial specialization" partialSpecialization
        , testCase "total specialization" totalSpecialization
        , testCase "application expression" applicatioSpecialization
        ]
  where
    -- TODO: Test that it fails if rule to specialize is not in the context
    partialSpecialization = do
        let (a, st) =
                runProofM
                    (specialize ("mp" ## "X") "Hs")
                    (ProofState
                        { goal = "Y"
                        , premises = []
                        , env =
                            M.fromList
                                [ ("mp", Rule "mp" ["P", "Q"] (FromDerive ["P", "impl" ## "P" ## "Q"] "Q"))
                                , ("X", Rule "X" [] "X")
                                ]
                        }
                    )
        assertBool "specialize result is in environment" $ isJust $ st ^. _env . at "Hs"
        let Just (Rule name args exp) = st ^. _env . at "Hs"
        name @?= "Hs"
        args @?= ["Q"]
        exp @?= FromDerive ["X", "impl" ## "X" ## "Q"] "Q"

    totalSpecialization = do
        let (a, st) =
                runProofM
                    (specialize ("Hs" ## "Y") "Hss")
                    (ProofState
                        { goal = "Y"
                        , premises = []
                        , env =
                            M.fromList
                                [ ("mp", Rule "mp" ["P", "Q"] (FromDerive ["P", "impl" ## "P" ## "Q"] "Q"))
                                , ("X", Rule "X" [] "X")
                                , ("Y", Rule "Y" [] "Y")
                                , ("Hs", Rule "Hs" ["Q"] (FromDerive ["X", "impl" ## "X" ## "Q"] "Q"))
                                ]
                        }
                    )
        assertBool "specialize result is in environment" $ isJust $ st ^. _env . at "Hss"
        let Just (Rule name args exp) = st ^. _env . at "Hss"
        name @?= "Hss"
        args @?= []
        exp @?= FromDerive ["X", "impl" ## "X" ## "Y"] "Y"

    applicatioSpecialization = do
        let (a, st) =
                runProofM
                    (specialize ("r" ## "H") "Hss")
                    (ProofState
                        { goal = "Y"
                        , premises = []
                        , env =
                            M.fromList
                                [ ("r", Rule "r" ["P"] (FromDerive ["impl" ## "P" ## "P"] "P"))
                                , ("H", Rule "H" [] ("impl" ## "X" ## "Y"))
                                ]
                        }
                    )
        assertBool "specialize result is in environment" $ isJust $ st ^. _env . at "Hss"
        let Just (Rule name args exp) = st ^. _env . at "Hss"
        name @?= "Hss"
        args @?= []
        exp @?= FromDerive ["impl" ## ("impl" ## "X" ## "Y") ## ("impl" ## "X" ## "Y")] ("impl" ## "X" ## "Y")