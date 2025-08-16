{-# LANGUAGE FlexibleInstances #-}

module Azalea.Pretty (PP (..), prettySpan) where

import Azalea.AST (Function (..), ToplevelStmt (..))
import Azalea.AST.Expr (Expr (..), Lam (..), Literal (..), Member (..), Record (..))
import Azalea.AST.Span (Span (..), value)
import Azalea.AST.Stmt (Block, Stmt (..), ToplevelStmt)
import Azalea.AST.Types (Ty)
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Vector qualified as V
import Prettyprinter
import Prettyprinter.Render.String (renderString)
import Text.Megaparsec (SourcePos (..), unPos)

text :: String -> Doc ann
text = pretty . T.pack

-- | Typeclass for pretty printing AST nodes
class PP a where
  pp :: a -> Doc ann

instance PP Ty where
  pp = pretty . show

instance PP Block where
  pp block = vcat . map pp $ V.toList block

instance PP Expr where
  pp (ELit lit) = pp lit
  pp (EIdent name) = pretty name
  pp (EMember m) = text "Member" <+> pretty (memberName m)
  pp (EBinOp op l r) = parens $ pp l <+> (pretty . show $ op) <+> pp r
  pp (EUnaryOp op e) = parens (pretty . show $ op) <+> pp e
  pp (ERecord r) = text "Record" <+> pretty (recordName r)
  pp (ECall f args) = pp f <> tupled (map pp args)
  pp (EArrayIndex arr idx) = pp arr <> text "." <> brackets (pp idx)
  pp (ELam lam) = text "\\" <> hsep (punctuate comma (map pretty (M.keys (lamParams lam)))) <+> text "->" <+> pp (lamBody lam)
  pp (EIf cond thenExpr elseExpr) =
    text "if" <+> pp cond <+> text "then" <+> pp thenExpr <+> text "else" <+> pp elseExpr

instance PP (Span Expr) where
  pp = pretty . prettySpan

instance PP Literal where
  pp (IntLit n) = pretty n
  pp (FloatLit n) = pretty n
  pp (StringLit s) = dquotes (pretty s)
  pp (ArrayLit elems) = list (map (pp . value) elems)
  pp (BoolLit b) = pretty b

instance PP Member where
  pp (Member name target) = text "Member" <+> pretty name <+> text "of" <+> pp target

instance PP Record where
  pp (Record name fields) =
    text "Record" <+> pretty name <+> braces (hsep . punctuate comma $ map (\(k, v) -> pretty k <+> text "=" <+> pp v) (M.toList fields))

instance PP Lam where
  pp (Lam params body) =
    text "\\"
      <> hsep (punctuate comma (map pretty (M.keys params)))
        <+> text "->"
        <+> pp body

-- Statement instances
instance PP Stmt where
  pp (Let name ty expr) =
    text "let" <+> pretty name <+> text ":" <+> pp ty <+> text "=" <+> pp expr
  pp (Mut name ty expr) =
    text "mut" <+> pretty name <+> text ":" <+> pp ty <+> text "=" <+> pp expr
  pp (Assign name expr) =
    pretty name <> text ":=" <> pp expr
  pp (For name expr block) =
    text "for" <+> pretty name <+> text "in" <+> pp expr <+> text "do" <+> pp block
  pp (While cond block) =
    text "while" <+> pp cond <+> text "do" <+> pp block
  pp (ExprStmt expr) = pp expr

instance PP ToplevelStmt where
  pp (AStmt stmt) = pp stmt
  pp (RecordDecl record) = pp record
  pp (FnDecl fn) =
    text "fn"
      <+> pretty (funcName fn)
      <> parens (hsep (punctuate comma (map (\(k, v) -> pretty k <+> text ":" <+> pp v) (M.toList (funcParams fn)))))
        <+> text "->"
        <+> pp (funcReturnType fn)
        <+> text "do"
        <+> pp (funcBody fn)

prettySpan :: (PP a) => Span a -> String
prettySpan (Span val spos) =
  renderString (layoutPretty defaultLayoutOptions (pp val))
    ++ " @ "
    ++ line''
    ++ ":"
    ++ col
 where
  line'' = show . unPos $ sourceLine spos
  col = show . unPos $ sourceColumn spos