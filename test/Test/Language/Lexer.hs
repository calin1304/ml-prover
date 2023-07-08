module Test.Language.Lexer
    ( tests
    ) where

import           Test.Tasty       (TestTree, testGroup)
import           Test.Tasty.HUnit (testCase, (@?=))

import           Language.Lexer   (scanner, LexemeClass(..))

tests :: TestTree
tests =
    testGroup "Lexer"
        [ scannerTests
        ]

scannerTests :: TestTree
scannerTests =
    testGroup "scanner"
        [ testCase "integers" scannerIntegersTest
        , testCase "strings" scannerStringsTest
        ]

scannerIntegersTest = actual @?= expected
  where
    expected = Right $ map LInteger [1,2,3,4]
    actual = scanner "1 2 3 4"

scannerStringsTest = actual @?= expected
  where
    expected = Right $ map LString ["hello", "world"]
    actual = scanner "\"hello\" \"world\""
