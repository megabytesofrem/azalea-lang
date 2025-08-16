{-# LANGUAGE OverloadedRecordDot #-}

module Azalea.Parser
  ( parseExpr
  , parseExprSpanned
  , runParse
  -- Export the Parser type and custom error
  , Parser
  , ParserError (..)
  ) where

import Azalea.AST.Expr (Expr (..), Function (..), Lam (..), Literal (..), Record (..))
import Azalea.AST.Span (Span, mkSpanned)
import Control.Monad.Combinators.Expr

import Azalea.AST.Stmt (Stmt (..), ToplevelStmt (..))
import Azalea.AST.Types
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Text (Text, pack, unpack)
import Data.Vector qualified as V
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

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

reservedTypes :: [Text]
reservedTypes = ["Int", "Float", "String", "Bool", "Unit", "Array"]

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
    <|> userDefined
 where
  -- Parse user-defined types
  userDefined = do
    ident <- pIdent
    args <- optional (between (symbol "[") (symbol "]") (pType `sepBy` symbol ","))
    if ident `elem` reservedTypes
      then do
        pos <- getSourcePos
        customFailure (ParseError ("Reserved type " <> ident) pos)
      else pure (TyCons ident (maybe V.empty V.fromList args))

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

pRecord :: Parser Expr
pRecord = do
  pos <- getSourcePos
  ident <- pIdent <|> symbol "."
  fields <- between (symbol "{") (symbol "}") ((pField pos) `sepBy` symbol ",")
  pure $ ERecord (Record ident (M.fromList fields))

pField :: SourcePos -> Parser (Text, Span Expr)
pField pos = do
  fieldName <- pIdent
  _ <- symbol ":"
  fieldValue <- parseExpr
  pure (fieldName, mkSpanned fieldValue pos)

pTerm :: Parser Expr
pTerm =
  try
    (ELit <$> pLiteral) -- 12, "hello", [1,2,3]
    <|> try (parens parseExpr) -- Parenthesized expressions
    <|> (EIdent <$> pIdent) -- Identifiers
    <|> pIf -- If expressions
    <|> pLambda -- Lambda expressions
    <|> pRecord -- Record expressions

-- Statements
pLet :: Parser Stmt
pLet = do
  _pos <- getSourcePos
  _ <- symbol "let"
  name <- unpack <$> pIdent
  ty <- optional (symbol ":" >> pType)
  _ <- symbol "="
  expr <- parseExpr
  pure $ Let name (maybe TyUnknown id ty) expr

pMut :: Parser Stmt
pMut = do
  pos <- getSourcePos
  _ <- symbol "mut"
  name <- unpack <$> pIdent
  ty <- optional (symbol ":" >> pType)
  _ <- symbol "="
  expr <- parseExpr
  pure $ Mut name (maybe TyUnknown id ty) expr

pAssign :: Parser Stmt
pAssign = do
  pos <- getSourcePos
  name <- unpack <$> pIdent
  _ <- symbol "="
  expr <- parseExpr
  pure $ Assign name expr

pFor :: Parser Stmt
pFor = do
  pos <- getSourcePos
  _ <- symbol "for"
  iterVar <- unpack <$> pIdent
  _ <- symbol "in"
  iterExpr <- parseExpr
  body <- parseBlock
  pure $ For iterVar iterExpr body

pRecordDecl :: Parser ToplevelStmt
pRecordDecl = do
  pos <- getSourcePos
  _ <- symbol "record"
  name <- pIdent
  _ <- symbol "="
  fields <- between (symbol "{") (symbol "}") ((pField pos) `sepBy` symbol ",")
  pure $ RecordDecl (Record name (M.fromList fields))

pFnParams :: Parser [(Text, Ty)]
pFnParams = parensArg
 where
  pArg = do
    ident <- pIdent
    mty <- optional (symbol ":" >> pType)
    pure (ident, maybe TyUnknown id mty)

  parensArg = between (symbol "(") (symbol ")") (pArg `sepBy` symbol ",")

pFnDeclWithBlock :: Parser ToplevelStmt
pFnDeclWithBlock = do
  pos <- getSourcePos
  _ <- symbol "fn"
  name <- pIdent
  args <- pFnParams
  let args' = M.fromList args
  retTy <- optional (symbol "->") *> optional (symbol ":" >> pType)
  body <- parseBlock
  pure $ mkFunc name args' retTy body
 where
  mkFunc :: Text -> M.Map Text Ty -> Maybe Ty -> V.Vector Stmt -> ToplevelStmt
  mkFunc name args' retTy body
    | retTy == Nothing = FnDecl (Function name args' TyUnknown body)
    | otherwise = FnDecl (Function name args' (fromJust retTy) body)

pFnDeclWithExpr :: Parser ToplevelStmt
pFnDeclWithExpr = do
  pos <- getSourcePos
  _ <- symbol "fn"
  name <- pIdent
  args <- pFnParams
  let args' = M.fromList args
  retTy <- optional (symbol "->") *> optional (symbol ":" >> pType)
  _ <- symbol "="
  expr <- parseExpr
  pure $ mkFunc name args' retTy expr
 where
  mkFunc :: Text -> M.Map Text Ty -> Maybe Ty -> Expr -> ToplevelStmt
  mkFunc name args' retTy body
    | retTy == Nothing = FnDecl (Function name args' TyUnknown (V.singleton (ExprStmt body)))
    | otherwise = FnDecl (Function name args' (fromJust retTy) (V.singleton (ExprStmt body)))

pFnDecl :: Parser ToplevelStmt
pFnDecl =
  try pFnDeclWithBlock <|> try pFnDeclWithExpr

-- Parse a block of statements
parseBlock :: Parser (V.Vector Stmt)
parseBlock = do
  _ <- symbol "do"
  stmts <- many (parseStmt <* spaceConsumer)
  _ <- symbol "end"
  pure $ V.fromList stmts

-- Parse a statement (let, mut, assign, for, or expression statement)
parseStmt :: Parser Stmt
parseStmt =
  try pLet
    <|> try pMut
    <|> try pAssign
    <|> try pFor
    <|> try (ExprStmt <$> parseExpr)

-- Parse a top-level statement, which can be a function, record declaration, or a statement
parseToplevelStmt :: Parser ToplevelStmt
parseToplevelStmt =
  try pFnDecl
    <|> try pRecordDecl
    <|> try pFnDecl
    <|> (AStmt <$> parseStmt)

parseExpr :: Parser Expr
parseExpr = makeExprParser pTerm operators

-- `parseExpr`, but spanned with source position
parseExprSpanned :: Parser (Span Expr)
parseExprSpanned = do
  pos <- getSourcePos
  expr <- parseExpr
  pure $ mkSpanned expr pos

-- Run the parser on a given input string
runParse :: Text -> Either (ParseErrorBundle Text ParserError) ToplevelStmt
runParse input =
  parse parseToplevelStmt "<input>" input