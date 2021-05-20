module Test.Language.Parser where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Language.Syntax
import Language.Lexer
import Language.Parser
import Language.ParserM

tests :: TestTree
tests = testGroup "Parser"
    [ testCase
        "can parse application"
        test_parse_application
    ]

test_parse_application = actual @?= expected
  where
    expected =
        Right
            $ Application
                (Application
                    ( Application
                        ( Application (Ident "f") (Ident "X")
                        ) (Ident "Y")
                    ) (Ident "Z")
                )
                (Application (Application (Ident "g") (Ident "X")) (Ident "Y"))
    actual = runParserM parseExpression <$> scanner "(f X Y Z) (g X Y)"
