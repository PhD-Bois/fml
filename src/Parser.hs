{-# LANGUAGE OverloadedStrings #-}

module Parser
    ( parseUnit
    ) where

import Control.Applicative (liftA2)
import Control.Monad (void)

import Data.HashSet (HashSet)
import qualified Data.HashSet as Set

import Data.Text (Text)
import qualified Data.Text as T

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Text (Parser)

data AST
    = Identifier Text
    | Typename Text


uppercase :: String
uppercase = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

lowercase :: String
lowercase = "_abcdefghijklmnopqrstuvwxyz"

numeric :: String
numeric = "1234567890"

alphanum :: String
alphanum = lowercase ++ uppercase ++ numeric

symbol :: String
symbol = "!$%&/\\=?+*#<>-^"


-- maybe we should check if String or Text is faster for HashSet?
keywords :: HashSet Text
keywords = Set.fromList ["=", "and", "in", "let", "sig"]

parseUnit :: Text -> Either ParseError [()]
parseUnit text = parse (spaces *> unit) "test" text

unit :: Parser [()]
unit = many (definition <|> signature)

-- top level let bindings
definition :: Parser ()
definition = keyword "let"

signature :: Parser ()
signature = keyword "sig"

typename :: Parser AST
typename = lexeme $ do
    name <- T.pack <$> liftA2 (:) (oneOf lowercase) (many (oneOf alphanum))
    if Set.member name keywords
        then unexpected "keyword"
        else Typename <$> pure name

identifier :: Parser AST
identifier = lexeme $ do
    name <- T.pack <$> liftA2 (:) (oneOf lowercase) (many (oneOf alphanum))
    if Set.member name keywords
        then unexpected "keyword"
        else Identifier <$> pure name

operator :: Parser AST
operator = lexeme $ do
    name <- T.pack <$> many1 (oneOf symbol)
    if Set.member name keywords
        then unexpected "keyword"
        else Identifier <$> pure name

keyword :: String -> Parser ()
keyword k = lexeme $ void (string k)  -- maybe add runtime assertion that k is indeed a keyword?

lexeme :: Parser a -> Parser a
lexeme = (<* spaces)
