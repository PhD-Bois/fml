{-# LANGUAGE OverloadedStrings #-}

module Parser
    ( parseUnit
    ) where

import Control.Applicative (liftA2)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (isJust)

import qualified Data.Text as Text

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Text (Parser)

import IR
import Parser.Combinators

parseUnit :: String -> Text.Text -> Either ParseError Unit
parseUnit = parse (spaces *> unit <* eof)

unit :: Parser Unit
unit = Unit <$> many (dataDefinition <|> typeAlias <|> letDefinition <|> signature)

dataDefinition :: Parser TopLevel
dataDefinition = do
    keyword "data"
    ty <- typeExpression
    constructors <- many single
    pure $ DataDefinition ty constructors
  where
    single = keyword "|" *> liftA2 (,) typeExpression (optionMaybe $ keyword ":" *> typeSignature)

typeAlias :: Parser TopLevel
typeAlias = do
    keyword "type"
    alias <- typeExpression
    keyword "="
    ty <- typeSignature
    pure $ TypeAlias alias ty

-- top level let bindings
letDefinition :: Parser TopLevel
letDefinition = do
    keyword "let"
    rec <- isJust <$> (optionMaybe $ try (keyword "rec"))
    args <- many1 pattern
    keyword "="
    expr <- expression
    andBindings <- many letAnd
    pure $ Definition ((rec, NonEmpty.fromList args, expr) :| andBindings)

signature :: Parser TopLevel
signature = do
    keyword "sig"
    name <- identifier
    keyword ":"
    ty <- typeSignature
    pure $ Signature name ty


typeExpression :: Parser Type
typeExpression = do
    ls <- many1 single
    pure $ case ls of
        [x] -> x
        (x : xs) -> TypeApp x xs
        _ -> error "unreachable"
  where
    single = recordType
        <|> try (fmap Typename typename)
        <?> "type"

recordType :: Parser Type
recordType = do
    punct "{"
    fields <- field `sepEndBy` punct ","
    row <- optionMaybe $ keyword "|" *> identifier
    punct "}"
    pure $ RecordType fields row
  where
    field = do
        name <- identifier
        keyword ":"
        ty <- typeSignature
        pure (name, ty)

-- type expression, used in
typeSignature :: Parser Type
typeSignature = do
    ls <- many1 single
    pure $ case ls of
        [x] -> x
        (x : xs) -> TypeApp x xs
        _ -> error "unreachable"
  where
    single = punct "(" *> typeSignature <* punct ")"
        <|> recordType
        <|> try (fmap Typename typename)
        <?> "type"

pattern :: Parser Pattern
pattern = Ref <$> identifier

expression :: Parser Expression
expression = do
    ls <- many1 single
    pure $ case ls of
        [x] -> x
        (x : xs) -> Application x xs
        _ -> error "unreachable"
  where
    single = punct "(" *> expression <* punct ")"
        <|> lambda
        <|> record
        <|> fmap LiteralExpr literal
        <|> letExpression
        <|> ifExpression
        <|> try (fmap Var identifier)
        <?> "expression"

lambda :: Parser Expression
lambda = do
    try $ keyword "\\"
    pats <- NonEmpty.fromList <$> many1 pattern
    keyword "=>"
    expr <- expression
    pure $ Lambda pats expr

record :: Parser Expression
record = do
    punct "{"
    fields <- field `sepEndBy` punct ","
    row <- optionMaybe $ keyword "|" *> expression
    punct "}"
    pure $ RecordLiteral fields row
  where
    field = do
        name <- identifier
        keyword "="
        expr <- expression
        pure (name, expr)

letExpression :: Parser Expression
letExpression = do
    try $ keyword "let"
    rec <- isJust <$> (optionMaybe $ try (keyword "rec"))
    args <- many1 pattern
    keyword "="
    expr <- expression
    andBindings <- many letAnd
    keyword "in"
    inExpr <- expression
    pure $ Let ((rec, NonEmpty.fromList args, expr) :| andBindings) inExpr

letAnd :: Parser (Rec, NonEmpty Pattern, Expression)
letAnd = do
    keyword "and"
    rec <- isJust <$> (optionMaybe $ try (keyword "rec"))
    args <- many1 pattern
    keyword "="
    expr <- expression
    pure (rec, NonEmpty.fromList args, expr)

ifExpression :: Parser Expression
ifExpression = do
    try $ keyword "if"
    cond <- expression
    keyword "then"
    ifTrue <- expression
    keyword "else"
    ifFalse <- expression
    pure $ If cond ifTrue ifFalse

-- TODO
literal :: Parser Literal
literal = lexeme $ IntLiteral . read <$> many1 digit
    <|> CharLiteral <$> (punct "'" *> noneOf "'" <* punct "'")  -- TODO: escape chars
    <|> StringLiteral <$> (punct "\"" *> many (noneOf "\"") <* punct "\"")  -- TODO: escape chars
