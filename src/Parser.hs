{-# LANGUAGE OverloadedStrings #-}

module Parser
    ( parseUnit
    ) where

import Control.Applicative (liftA2)
import Control.Monad (void, when)
import Data.Char (isAsciiLower, isAsciiUpper)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (isJust)

import Data.HashSet (HashSet)
import qualified Data.HashSet as Set

import qualified Data.Text as Text

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Text (Parser)

import IR
import Parser.Combinators

parseUnit :: String -> Text.Text -> Either ParseError Unit
parseUnit = parse (spaces *> unit <* eof)

unit :: Parser Unit
unit = Unit <$> many (dataDefinition <|> definition <|> signature)

dataDefinition :: Parser TopLevel
dataDefinition = do
    keyword "data"
    ty <- typeExpression
    constructors <- many single
    pure $ DataDefinition ty constructors
  where
    single = keyword "|" *> liftA2 (,) typeExpression (optionMaybe $ keyword ":" *> typeSignature)

typeExpression :: Parser Type
typeExpression = do
    ls <- many1 (try (fmap Typename typename) <?> "type")
    pure $ case ls of
        [x] -> x
        (x : xs) -> TypeApp x xs
        _ -> error "unreachable"

-- top level let bindings
-- TODO: add rec
definition :: Parser TopLevel
definition = do
    keyword "let"
    rec <- isJust <$> (optionMaybe $ try (keyword "rec"))
    name <- identifier
    args <- many pattern
    keyword "="
    expr <- expression
    pure $ Definition rec name args expr

signature :: Parser TopLevel
signature = do
    keyword "sig"
    name <- identifier
    keyword ":"
    ty <- typeSignature
    pure $ Signature name ty


-- type expression, used in
typeSignature :: Parser Type
typeSignature = do
    ls <- many1 single
    pure $ case ls of
        [x] -> x
        (x : xs) -> TypeApp x xs
        _ -> error "unreachable"
  where
    single = lexeme (char '(') *> typeSignature <* lexeme (char ')')
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
    single = lexeme (char '(') *> expression <* lexeme (char ')')
        <|> lambda
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

-- TODO: add rec
letExpression :: Parser Expression
letExpression = do
    try $ keyword "let"
    rec <- isJust <$> (optionMaybe $ try (keyword "rec"))
    pat <- pattern
    keyword "="
    expr <- expression
    bindings <- many letAnd
    keyword "in"
    inExpr <- expression
    pure $ Let ((rec, pat, expr) :| bindings) inExpr
  where
    letAnd = do
        keyword "and"
        rec <- isJust <$> (optionMaybe $ try (keyword "rec"))
        pat <- pattern
        keyword "="
        expr <- expression
        pure (rec, pat, expr)

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
literal = IntLiteral . read <$> many1 digit
