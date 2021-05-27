module Language.Syntax where

import           Data.List        (intercalate)
import           Data.String      (IsString, fromString)
import           Test.QuickCheck
import           Text.PrettyPrint (($+$), (<+>))
import qualified Text.PrettyPrint as PP
import           Text.Printf      (printf)

import           Language.Lexer
import           Print

newtype Source = Source [ModDef]
    deriving (Show)

data ModDef = ModDef String [Declaration]

instance Show ModDef where
    show = PP.render . docModDef

docModDef :: ModDef -> PP.Doc
docModDef (ModDef name decls) =
    docName <+> PP.lbrace $+$ PP.nest 4 docDecls $+$ PP.rbrace
  where
    docName = PP.doubleQuotes (PP.text name)
    docDecls = PP.vcat $ map docDeclaration decls

data Declaration =
    MetaSym
        String    -- ^ Meta symbol name
        [SymAttr] -- ^ Meta symbol attributes
  | Notation
        String    -- ^ Notation name
        Signature -- ^ Notation arguments
        Expr      -- ^ Notation expression
        [SymAttr] -- ^ Notation attributes
  | Import
        String -- ^ Import name
  | Rule
        String   -- ^ Rule name
        [String] -- ^ Argument names
        [Expr]   -- ^ Hypotheses
        Expr     -- ^ Conclusion
  | Lemma
        String   -- ^ Lemma name
        [String] -- ^ Argument names
        [Expr]   -- ^ Hypotheses
        Expr     -- ^ Conclusion
        [Tactic] -- ^ Proof
    deriving (Eq)

axiom :: String -> Expr -> Declaration
axiom name = Rule name [] []

instance Show Declaration where
    show = PP.render . docDeclaration

class HasDefinition a where
    getDefinition :: a -> ([Expr], Expr)

instance HasDefinition Declaration where
    getDefinition (Rule _ _ hs e) = (hs, e)
    getDefinition _               = undefined

data Tactic =
    Intros String
  | Specialize Expr String
  | Apply Expr (Maybe String)
  | Exact String
    deriving (Eq, Show)

data Expr =
    Ident String
  | Application Expr Expr
    deriving (Eq)

instance Show Expr where
    show = PP.render . docExpr

instance IsString Expr where
    fromString s = Ident s

-- I'm tired of writing long names
infixl 5 ##
(##) = Application

data Signature =
    NoSignature
  | Signature [Argument]
    deriving (Eq, Show)

newtype Argument = Argument String
    deriving (Eq, Show)

data SymAttr =
    Folded
  | Substitution
  | Binder
  | SetBinder
  | NotNegative
  | Arity Int
    deriving (Eq, Show)

mkAttr :: String -> [Int] -> SymAttr
mkAttr name args =
    case name of
        "folded"       -> Folded
        "binder"       -> Binder
        "substitution" -> Substitution
        "arity"        -> Arity (head args)
        "set-binder"   -> SetBinder
        "notNegative"  -> NotNegative
        _              -> error $ "Invalid attribute name: " ++ name

---------------------
-- Pretty printing --
---------------------

docDeclaration :: Declaration -> PP.Doc
docDeclaration = \case
    MetaSym name _ -> PP.text name
    Notation name _ def _ -> docDef name def
    Import name -> PP.text name
    Rule name args hs c ->
        let name' = PP.text name
            args' = PP.text $ unwords args
            hypotheses = docExprs hs
            conclusion = docExpr c
         in if null args
                then PP.hsep [name', definition, hypotheses, vdash, conclusion]
                else PP.hsep [name', definition, forall, args', PP.colon, hypotheses, vdash, conclusion]
    Lemma name args hs c proof ->
        let docProof = PP.vcat $ map docTactic proof
            args' = PP.hsep $ map PP.text args
            hypotheses = PP.text "_"
            conclusion = PP.text "_"
         in  PP.hsep
                [ PP.text ("L_" <> name)
                , definition
                , forall
                , args'
                , PP.colon
                , hypotheses
                , conclusion
                ]
                $+$ PP.nest 4 (docProof $+$ qed)


docDef :: String -> Expr -> PP.Doc
docDef name expr  = PP.text name <+> definition <+> docExpr expr

docExpr :: Expr -> PP.Doc
docExpr = \case
    Ident s -> if s == "top" then top else PP.text s
    Application e1 e2 ->
        let e2' = (if isApplication e2 then PP.parens else id) (docExpr e2)
         in docExpr e1 <+> e2'
  where
    isApplication :: Expr -> Bool
    isApplication (Application _ _) = True
    isApplication _                 = False

docExprs :: [Expr] -> PP.Doc
docExprs xs = PP.brackets $ PP.hcat (PP.punctuate PP.comma (map docExpr xs))

docTactic :: Tactic -> PP.Doc
docTactic = \case
    Intros asName -> PP.text "intros" <+> PP.text asName
    Specialize e asName -> PP.text "specialize" <+> docExpr e <+> PP.text asName
    Apply e maybeAsName -> PP.text "apply" <+> docExpr e <+> PP.text (show maybeAsName)
    Exact name -> PP.text "exact" <+> PP.text name
