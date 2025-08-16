{-# LANGUAGE FlexibleInstances #-}

import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Text qualified as T

-- Use golden tests (snapshot testing) to verify the test results

import Azalea.Parser (pType, runParse)
import Azalea.Pretty (PP (..))
import Prettyprinter
import Prettyprinter.Render.String (renderString)
import Test.Tasty
import Test.Tasty.Golden (goldenVsString)
import Text.Megaparsec (parse)

-- | Create a golden test/snapshot that includes the source code and the result
snapSource :: (Show e, PP t) => TestName -> FilePath -> String -> Either e t -> TestTree
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
      [ exprParserTests
      , stmtParserTests
      ]

-- Pretty printing functions for AST nodes
-- Should probably move these into a seperate module soon

-- NOTE: <+> inserts whitespace, <> concatenates without whitespace

prettyResult :: (Show e, PP t) => Either e t -> String
prettyResult (Left err) = "Error: " ++ show err
prettyResult (Right val) = renderString . layoutPretty defaultLayoutOptions $ pp val

-- Test cases for the parser
exprParserTests :: TestTree
exprParserTests =
  testGroup
    "parser"
    [ -- Type parsing tests
      snapSource
        "parseExpr Int"
        "test/golden/parse-type-01.golden"
        "Int"
        (parse pType "<input>" (T.pack "Int"))
    , snapSource
        "parseExpr List[Int]"
        "test/golden/parse-type-02.golden"
        "List[Int]"
        (parse pType "<input>" (T.pack "List[Int]"))
    , snapSource
        "parseExpr List[List[Int]]"
        "test/golden/parse-type-03.golden"
        "List[List[Int]]"
        (parse pType "<input>" (T.pack "List[List[Int]]"))
    , -- Int parsing tests
      snapSource
        "parseExpr 123"
        "test/golden/parse-int-123.golden"
        "123"
        (runParse (T.pack "123"))
    , -- Fails as expected
      snapSource
        "parseExpr -123, should fail"
        "test/golden/parse-int-negative.golden"
        "-123"
        (runParse (T.pack "-123"))
    , -- Float parsing tests
      snapSource
        "parseExpr 123.456"
        "test/golden/parse-float-accurate.golden"
        "123.456"
        (runParse (T.pack "123.456"))
    , snapSource
        "parseExpr 0.123"
        "test/golden/parse-float.golden"
        "0.123"
        (runParse (T.pack "0.123"))
    , -- Array parsing tests
      snapSource
        "parseExpr [1, 2, 3]"
        "test/golden/parse-array.golden"
        "[1, 2, 3]"
        (runParse (T.pack "[1, 2, 3]"))
    , snapSource
        "parseExpr []"
        "test/golden/parse-array-empty.golden"
        "[]"
        (runParse (T.pack "[]"))
    , snapSource
        "parseExpr items.[1]"
        "test/golden/parse-array-index.golden"
        "items.[1]"
        (runParse (T.pack "items.[1]"))
    , -- Parentheses parsing tests
      snapSource
        "parseExpr (1 + 2 * (3 - 4))"
        "test/golden/parse-parentheses.golden"
        "(1 + 2 * (3 - 4))"
        (runParse (T.pack "(1 + 2 * (3 - 4))"))
    , -- Lambda parsing tests
      snapSource
        "parseExpr \\x -> x + 1"
        "test/golden/parse-lambda.golden"
        "\\x -> x + 1"
        (runParse (T.pack "\\x -> x + 1"))
    , snapSource
        "parseExpr \\(x, y) -> x + y"
        "test/golden/parse-lambda-multiple-args.golden"
        "\\(x, y) -> x + y"
        (runParse (T.pack "\\(x, y) -> x + y"))
    ]

stmtParserTests :: TestTree
stmtParserTests =
  testGroup
    "stmt-parser"
    [ snapSource
        "parseStmt let x = 1"
        "test/golden/parse-let.golden"
        "let x = 1"
        (runParse (T.pack "let x = 1"))
    , snapSource
        "parseStmt let x: Int = 1"
        "test/golden/parse-let-typed.golden"
        "let x: Int = 1"
        (runParse (T.pack "let x: Int = 1"))
    , snapSource
        "parseStmt mut x = 1"
        "test/golden/parse-mut.golden"
        "mut x = 1"
        (runParse (T.pack "mut x = 1"))
    , snapSource
        "parseStmt mut x: Int = 1"
        "test/golden/parse-mut-typed.golden"
        "mut x: Int = 1"
        (runParse (T.pack "mut x: Int = 1"))
    , snapSource
        "parseStmt x = 1"
        "test/golden/parse-assign.golden"
        "x = 1"
        (runParse (T.pack "x = 1"))
    , -- For and While loops
      snapSource
        "parseStmt for x in 1..10 do x + 1 end"
        "test/golden/parse-for.golden"
        "for x in xs do x + 1 end"
        (runParse (T.pack "for x in xs do x + 1 end"))
    , snapSource
        "parseStmt while x < 10 do x + 1 end"
        "test/golden/parse-while.golden"
        "while x < 10 do x + 1 end"
        (runParse (T.pack "while x < 10 do x + 1 end"))
    ]
