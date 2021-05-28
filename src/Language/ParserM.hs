module Language.ParserM
    ( ParserM
    , ParserState(..)
    , runParserM
    , getName
    , hasName
    ) where

import           Control.Monad.State (State, evalState, gets)
import           Data.Maybe          (isJust)
import qualified Text.PrettyPrint    as PP

import           Language.Lexer      (LexemeClass)
import           Language.Syntax     (Declaration)
import           Pretty              (Pretty, pretty)
import           Utils               ()

type ParserM a = State ParserState a

newtype ParserState = ParserState
    { names :: [(String, Declaration)]
    }
    deriving (Show)

instance Pretty ParserState where
    pretty (ParserState names) = PP.braces $ PP.nest 4 docNames
      where
        docNames = PP.vcat $ map (PP.text . show . snd) names

runParserM :: ([LexemeClass] -> ParserM a)-> [LexemeClass] -> a
runParserM p ls = evalState (p ls) emptyState
  where
    emptyState = ParserState []

getName :: String -> ParserM (Maybe Declaration)
getName name = gets (lookup name . names)

hasName :: String -> ParserM Bool
hasName = fmap isJust . getName
