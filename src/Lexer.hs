module Lexer where

import qualified Data.Text as Text

import IR

data Token
    = IdentifierToken Identifier
    | TypenameToken Text.Text
    | Operator Text.Text
    | Keyword Text.Text
    | LiteralToken Literal
