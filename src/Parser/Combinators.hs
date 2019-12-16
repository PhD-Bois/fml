module Parser.Combinators
  ( keyword
  , typename
  , identifier
  , operator
  , lexeme
  ) where

import Control.Applicative (liftA2)
import Control.Monad (void, when)
import Data.Char (isAsciiLower, isAsciiUpper)

import qualified Data.HashSet as Set
import qualified Data.Text as Text
import Text.Parsec
import Text.Parsec.Text (Parser)

import IR

-- maybe we should check if String or Text is faster for HashSet?
keywords :: Set.HashSet String
keywords = Set.fromList ["and", "in", "let", "sig", "=", ":", "\\"]

symbol :: Parser Char
symbol = oneOf "!$%&/\\=?+*#<>-^" <?> "symbol"

lowercase :: Parser Char
lowercase = satisfy (liftA2 (||) isAsciiLower (== '_')) <?> "lowercase letter or '_'"

uppercase :: Parser Char
uppercase = satisfy isAsciiUpper <?> "uppercase letter"

alphanum :: Parser Char
alphanum = lowercase <|> uppercase <|> digit

-- TODO
typename :: Parser Identifier
typename = lexeme $ do
    name <- liftA2 (:) uppercase (many alphanum)
    Identifier . Text.pack <$> pure name

identifier :: Parser Identifier
identifier = lexeme $ do
    name <- liftA2 (:) lowercase (many alphanum)
    if Set.member name keywords
        then unexpected "keyword"
        else Identifier . Text.pack <$> pure name

operator :: Parser Identifier
operator = lexeme $ do
    name <- many1 symbol
    if Set.member name keywords
        then unexpected "keyword"
        else Identifier . Text.pack <$> pure name

-- TODO: add support for operators
keyword :: String -> Parser ()
keyword key = if key `elem` keywords
    then lexeme (void (string key)) <?> "keyword '" ++ key ++ "'"
    else error "Parser.Combinators.keyword: expected keyword"

-- parses additional trailing whitespaces
lexeme :: Parser a -> Parser a
lexeme = (<* spaces)
