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

-- maybe we should check if String or Text is faster for HashSet?
keywords :: HashSet Text
keywords = Set.fromList ["let"]

parseUnit :: Text -> Either ParseError ()
parseUnit text = parse unit "test" text

unit :: Parser ()
unit = eof

-- let bindings
definition :: Parser ()
definition = eof  -- temp

identifier :: Parser ()
identifier = eof  -- temp


-- parses the string and whitespace after it
lexeme :: String -> Parser Text
lexeme s = fmap T.pack (string s) <* spaces
