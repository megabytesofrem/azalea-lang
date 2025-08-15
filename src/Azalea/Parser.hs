{-# LANGUAGE OverloadedRecordDot #-}

module Azalea.Parser
  ( parseExpr
  -- Export the Parser type and custom error
  , Parser
  , ParserError (..)
  ) where

import Azalea.AST.Expr (Expr (..), Literal (..))
import Azalea.AST.Span (mkSpanned)
import Azalea.AST.Types (BOp (..), Lam (..), Ty (..))
import Control.Monad.Combinators.Expr

import Data.Map qualified as M
import Data.Text (Text, pack, unpack)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

-- Custom parser error type
data ParserError = ParseError
  { errorMessage :: Text
  , errorPosition :: SourcePos
  }
  deriving (Show, Eq, Ord)

instance ShowErrorComponent ParserError where
  showErrorComponent err =
    "Parse error at " ++ show (err.errorPosition) ++ ": " ++ unpack (err.errorMessage)

-- Our custom parser type
type Parser = Parsec ParserError Text

-- Ignore whitespace and comments in the parser
spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment "--") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

-- Parse a single identifier
pIdent :: Parser Text
pIdent = lexeme $ do
  first <- letterChar
  rest <- many (alphaNumChar <|> char '_')
  pure . pack $ (first : rest)

pType :: Parser Ty
pType =
  (symbol "Int" >> pure TyInt)
    <|> (symbol "Float" >> pure TyFloat)
    <|> (symbol "String" >> pure TyString)
    <|> (symbol "Bool" >> pure TyBool)
    <|> (symbol "Unit" >> pure TyUnit)
    <|> (symbol "Array" >> symbol "[" *> (TyArray <$> pType) <* symbol "]")

pIntLit :: Parser Literal
pIntLit = do
  int <- lexeme . some $ digitChar
  case reads (unpack . pack $ int) of
    [(n, "")] -> pure . IntLit $ n
    _ -> do
      pos <- getSourcePos
      customFailure (ParseError "Invalid integer format" pos)

pFloatLit :: Parser Literal
pFloatLit = do
  flt <- lexeme $ do
    digits1 <- some digitChar
    _ <- char '.'
    digits2 <- some digitChar
    pure (digits1 ++ "." ++ digits2)

  -- Parse the float and check if it is valid
  case reads (unpack . pack $ flt) of
    [(n, "")] -> pure . FloatLit $ n
    _ -> do
      pos <- getSourcePos
      customFailure (ParseError "Invalid float format" pos)

pStringLit :: Parser Literal
pStringLit = do
  _ <- char '"'
  content <- manyTill ch (char '"')
  pure . StringLit . pack $ content
 where
  -- Parse a character, allowing for escapes
  ch = escape <|> satisfy (\c -> c /= '"' && c /= '\\')
  escape = do
    _ <- char '\\'
    c <- anySingle
    case c of
      '"' -> pure '"'
      '\\' -> pure '\\'
      '0' -> pure '\0'
      'n' -> pure '\n'
      't' -> pure '\t'
      'r' -> pure '\r'
      _ -> pure c

pArrayLit :: Parser Literal
pArrayLit = do
  pos <- getSourcePos
  elems <- between (symbol "[") (symbol "]") (parseExpr `sepBy` symbol ",")
  pure . ArrayLit $ (map (\e -> mkSpanned e pos) elems)

pLiteral :: Parser Literal
pLiteral = try pFloatLit <|> pIntLit <|> pStringLit <|> pArrayLit

postfixOps :: [Operator Parser Expr]
postfixOps =
  [ postfix pCall
  , postfix pIndex
  ]
 where
  postfix = Postfix

  -- Function calls are handled as postfix operators
  pCall :: Parser (Expr -> Expr)
  pCall = do
    args <- between (symbol "(") (symbol ")") (parseExpr `sepBy` symbol ",")
    pure $ \func -> ECall func args

  pIndex :: Parser (Expr -> Expr)
  pIndex = do
    _ <- symbol "."
    idx <- between (symbol "[") (symbol "]") parseExpr
    pure $ \arr -> EArrayIndex arr idx

-- TODO: Handle unary operators
operators :: [[Operator Parser Expr]]
operators =
  [
    [ InfixL (symbol "*" >> pure (\lhs rhs -> EBinOp Mul lhs rhs))
    , InfixL (symbol "/" >> pure (\lhs rhs -> EBinOp Div lhs rhs))
    ]
  ,
    [ InfixL (symbol "+" >> pure (\lhs rhs -> EBinOp Add lhs rhs))
    , InfixL (symbol "-" >> pure (\lhs rhs -> EBinOp Sub lhs rhs))
    ]
  ,
    [ InfixL (symbol "==" >> pure (\lhs rhs -> EBinOp Eq lhs rhs))
    , InfixL (symbol "!=" >> pure (\lhs rhs -> EBinOp Neq lhs rhs))
    , InfixL (symbol ">" >> pure (\lhs rhs -> EBinOp Gt lhs rhs))
    , InfixL (symbol "<" >> pure (\lhs rhs -> EBinOp Lt lhs rhs))
    , InfixL (symbol ">=" >> pure (\lhs rhs -> EBinOp Gte lhs rhs))
    , InfixL (symbol "<=" >> pure (\lhs rhs -> EBinOp Lte lhs rhs))
    , InfixL (symbol "+=" >> pure (\lhs rhs -> EBinOp PlusEq lhs rhs))
    , InfixL (symbol "-=" >> pure (\lhs rhs -> EBinOp MinusEq lhs rhs))
    ]
  ,
    [ InfixL (symbol "&&" >> pure (\lhs rhs -> EBinOp And lhs rhs))
    , InfixL (symbol "||" >> pure (\lhs rhs -> EBinOp Or lhs rhs))
    ]
  , -- Postfix
    postfixOps
  ]

pIf :: Parser Expr
pIf = do
  _pos <- getSourcePos
  _ <- symbol "if"
  cond <- parseExpr
  _ <- symbol "then"
  thenB <- parseExpr
  _ <- symbol "else"
  elseB <- parseExpr
  pure $ EIf cond thenB elseB

pLambda :: Parser Expr
pLambda = do
  pos <- getSourcePos
  _ <- symbol "\\"
  args <- parensArg <|> singleArg
  let args' = M.fromList args
  _ <- symbol "->"
  body <- parseExpr

  pure $ ELam (Lam args' (mkSpanned body pos))
 where
  parensArg = between (symbol "(") (symbol ")") (pArg `sepBy` symbol ",")
  -- Single argument has a unknown type
  singleArg = (: []) . (\ident -> (ident, TyUnknown)) <$> pIdent

  pArg :: Parser (Text, Ty)
  pArg = do
    ident <- pIdent
    mty <- optional (symbol ":" >> pType)
    pure (ident, maybe TyUnknown id mty)

pTerm :: Parser Expr
pTerm =
  try
    (ELit <$> pLiteral) -- 12, "hello", [1,2,3]
    <|> (EIdent <$> pIdent) -- Identifiers
    <|> pIf -- If expressions
    <|> pLambda -- Lambda expressions

parseExpr :: Parser Expr
parseExpr = makeExprParser pTerm operators
