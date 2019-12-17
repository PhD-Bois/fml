{-# LANGUAGE OverloadedStrings #-}

module Parser
    ( parseUnit
    ) where

import Control.Applicative (liftA2)
import Control.Monad (void, when)
import Data.Char (isAsciiLower, isAsciiUpper)
import qualified Data.List.NonEmpty as NonEmpty

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
unit = Unit <$> many (definition <|> signature)

-- top level let bindings
-- TODO: add rec
definition :: Parser TopLevel
definition = do
    keyword "let"
    name <- identifier
    args <- many pattern
    keyword "="
    expr <- expression
    pure $ Definition name args expr

signature :: Parser TopLevel
signature = do
    keyword "sig"
    name <- identifier
    keyword ":"
    -- TODO: type
    pure $ Signature name undefined


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
        <|> try (fmap Var identifier)
        <?> "expression"

lambda :: Parser Expression
lambda = do
    try $ keyword "\\"
    pats <- NonEmpty.fromList <$> many1 pattern
    keyword "->"
    expr <- expression
    pure $ Lambda pats expr

-- TODO: add rec
letExpression :: Parser Expression
letExpression = do
    try $ keyword "let"
    pat <- pattern
    keyword "="
    expr <- expression
    bindings <- many letAnd
    keyword "in"
    inExpr <- expression
    pure $ Let ((pat, expr) : bindings) inExpr
  where
    letAnd = do
        keyword "and"
        pat <- pattern
        keyword "="
        expr <- expression
        pure (pat, expr)

-- TODO
literal :: Parser Literal
literal = IntLiteral . read <$> many1 digit
