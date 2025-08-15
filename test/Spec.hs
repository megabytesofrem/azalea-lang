import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Text qualified as T

-- Use golden tests (snapshot testing) to verify the test results

import Azalea.AST.Expr (Expr (..), Literal (..), Member (..))
import Azalea.AST.Span (Span (..))

import Azalea.AST.Types (Record (recordName))

import Azalea.Parser (parseExpr)
import Prettyprinter
import Prettyprinter.Render.String (renderString)
import Test.Tasty
import Test.Tasty.Golden (goldenVsString)
import Text.Megaparsec (parse)

-- | Create a golden test/snapshot that includes the source code and the result
snapSource :: (Show e) => TestName -> FilePath -> String -> Either e Expr -> TestTree
snapSource testName goldenPath src result =
  goldenVsString
    testName
    goldenPath
    ( pure $
        BS.pack $
          "SOURCE:\n-------------\n" ++ src ++ "\n\nRESULT\n---------------\n" ++ pre
    )
 where
  pre = prettyResult result

main :: IO ()
main =
  defaultMain $
    testGroup
      "snapshots"
      [ parserTests
      ]

-- Pretty printing functions for AST nodes
-- Should probably move these into a seperate module soon

-- NOTE: <+> inserts whitespace, <> concatenates without whitespace

text :: String -> Doc ann
text = pretty . T.pack

prettyExpr :: Expr -> Doc ann
prettyExpr (ELit lit) = prettyLit lit
prettyExpr (EIdent name) = pretty name
prettyExpr (EMember m) = text "Member" <+> pretty (memberName m)
prettyExpr (EBinOp op l r) = parens (pretty . show $ op) <+> prettyExpr l <+> prettyExpr r
prettyExpr (EUnaryOp op e) = parens (pretty . show $ op) <+> prettyExpr e
prettyExpr (ERecord r) = text "Record" <+> pretty (recordName r)
prettyExpr (ECall f args) = prettyExpr f <> tupled (map prettyExpr args)
prettyExpr (EArrayIndex arr idx) = prettyExpr arr <> text "." <> brackets (prettyExpr idx)
prettyExpr (ELam lam) = text "Lam" <+> pretty (show lam)
prettyExpr (EIf cond thenExpr elseExpr) =
  text "if" <+> prettyExpr cond <+> text "then" <+> prettyExpr thenExpr <+> text "else" <+> prettyExpr elseExpr

prettyLit :: Literal -> Doc ann
prettyLit (IntLit n) = pretty n
prettyLit (FloatLit n) = pretty n
prettyLit (StringLit s) = dquotes (pretty s)
prettyLit (ArrayLit elems) = list (map (prettyExpr . value) elems)
prettyLit (BoolLit b) = pretty b

prettyResult :: (Show e) => Either e Expr -> String
prettyResult (Left err) = "Error: " ++ show err
prettyResult (Right val) = renderString . layoutPretty defaultLayoutOptions $ prettyExpr val

-- Test cases for the parser
parserTests :: TestTree
parserTests =
  testGroup
    "parser"
    [ -- Int parsing tests
      snapSource
        "parseExpr 123"
        "test/golden/parse-int-123.golden"
        "123"
        (parse parseExpr "<repl>" (T.pack "123"))
    , -- Fails as expected
      snapSource
        "parseExpr -123, should fail"
        "test/golden/parse-int-negative.golden"
        "-123"
        (parse parseExpr "<repl>" (T.pack "-123"))
    , -- Float parsing tests
      snapSource
        "parseExpr 123.456"
        "test/golden/parse-float-accurate.golden"
        "123.456"
        (parse parseExpr "<repl>" (T.pack "123.456"))
    , snapSource
        "parseExpr 0.123"
        "test/golden/parse-float.golden"
        "0.123"
        (parse parseExpr "<repl>" (T.pack "0.123"))
    , -- Array parsing tests
      snapSource
        "parseExpr [1, 2, 3]"
        "test/golden/parse-array.golden"
        "[1, 2, 3]"
        (parse parseExpr "<repl>" (T.pack "[1, 2, 3]"))
    , snapSource
        "parseExpr []"
        "test/golden/parse-array-empty.golden"
        "[]"
        (parse parseExpr "<repl>" (T.pack "[]"))
    , snapSource
        "parseExpr items.[1]"
        "test/golden/parse-array-index.golden"
        "items.[1]"
        (parse parseExpr "<repl>" (T.pack "items.[1]"))
    , -- Lambda parsing tests
      snapSource
        "parseExpr \\x -> x + 1"
        "test/golden/parse-lambda.golden"
        "\\x -> x + 1"
        (parse parseExpr "<repl>" (T.pack "\\x -> x + 1"))
    , snapSource
        "parseExpr \\(x, y) -> x + y"
        "test/golden/parse-lambda-multiple-args.golden"
        "\\(x, y) -> x + y"
        (parse parseExpr "<repl>" (T.pack "\\(x, y) -> x + y"))
    ]