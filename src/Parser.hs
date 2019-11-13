{-# LANGUAGE OverloadedStrings #-}

module Parser
    ( parseUnit
    ) where

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


-- maybe we should check if String or Text is faster for HashSet?
keywords :: HashSet Text
keywords = Set.fromList ["let", "sig"]

parseUnit :: Text -> Either ParseError [()]
parseUnit text = parse (spaces *> unit) "test" text

unit :: Parser [()]
unit = many definition

-- let bindings
definition :: Parser ()
definition = eof  -- temp

typename :: Parser AST
typename = lexeme $ do
    a <- oneOf uppercase
    b <- many (oneOf alphanum)
    let name = T.pack (a : b)
    if Set.member name keywords
        then unexpected "keyword"
        else Typename <$> pure name

identifier :: Parser AST
identifier = lexeme $ do
    a <- oneOf lowercase
    b <- many (oneOf alphanum)
    let name = T.pack (a : b)
    if Set.member name keywords
        then unexpected "keyword"
        else Identifier <$> pure name


lexeme :: Parser a -> Parser a
lexeme = (<* spaces)
