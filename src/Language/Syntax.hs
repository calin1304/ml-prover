module Language.Syntax where

import           Data.List       (intercalate)
import           Test.QuickCheck
import Text.Printf (printf)
import qualified Text.PrettyPrint as PP
import Text.PrettyPrint ((<+>), ($+$))

import           Language.Lexer
import Print

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
        String
        -- ^ Meta symbol name
        [SymAttr]
        -- ^ Meta symbol attributes
  | Notation
        String
        -- ^ Notation name
        Signature
        -- ^ Notation arguments
        Expr
        -- ^ Notation expression
        [SymAttr]
        -- ^ Notation attributes
  | Import
        String
        -- ^ Import name
  | Rule
        String
        -- ^ Rule name
        [String]
        -- ^ Argument names
        Expr
        -- ^ Definition
  | Lemma
        String
        -- ^ Lemma name
        [String]
        -- ^ Argument names
        Expr
        -- ^ Definition
        [Tactic]
        -- ^ Proof
    deriving (Eq)

instance Show Declaration where
    show = PP.render . docDeclaration

class HasDefinition a where
    getDefinition :: a -> Expr
instance HasDefinition Declaration where
    getDefinition (Rule _ _ e) = e
    getDefinition _ = undefined

data Tactic =
    Intros String
  | Specialize Expr String
  | Apply Expr (Maybe String)
  | Exact String
    deriving (Eq, Show)

data Expr =
    Ident String
  | Application Expr Expr
  | FromDerive [Expr] Expr
    deriving (Eq)

appPrec = 10

instance Show Expr where
    show = PP.render . docExpr

-- I'm tired of writing long names
infixl 5 ##
(##) = Application

data Signature =
    NoSignature
  | Signature [Argument]
    deriving (Eq, Show)

data Argument = Argument String
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
    Rule name args def ->
        if null args
            then docDef name def
            else PP.hsep [PP.text name, definition, forall, PP.text (intercalate " " args), PP.char ':', docExpr def]
    Lemma name args def proof ->
        let docProof = PP.vcat $ map docTactic proof
         in  PP.hsep
                [ PP.text ("L_" <> name)
                , definition
                , forall
                , PP.hsep $ map PP.text args
                , PP.colon
                , docExpr def
                ]
                $+$ PP.nest 4 (docProof $+$ qed)


docDef :: String -> Expr -> PP.Doc
docDef name expr = PP.text name <+> definition <+> docExpr expr

docExpr :: Expr -> PP.Doc
docExpr = \case
    Ident s -> if s == "top" then top else PP.text s
    Application e1 e2 -> docExpr e1 <+> docExpr e2
    FromDerive pres e ->
        let docPres = if null pres then PP.empty else PP.text (show pres) -- TODO: doc pres
         in docPres <+> vdash <+> docExpr e

docTactic :: Tactic -> PP.Doc
docTactic = \case
    Intros asName -> PP.text "intros" <+> PP.text asName
    Specialize e asName -> PP.text "specialize" <+> docExpr e <+> PP.text asName
    Apply e maybeAsName -> PP.text "apply" <+> docExpr e <+> PP.text (show maybeAsName)
    Exact name -> PP.text "exact" <+> PP.text name