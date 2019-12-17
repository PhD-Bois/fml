module IR
  ( Identifier(..)
  , Unit(..)
  , TopLevel(..)
  , Pattern(..)
  , Expression(..)
  , Literal(..)
  ) where

import Data.List.NonEmpty (NonEmpty)

import qualified Data.Text as Text

newtype Identifier = Identifier Text.Text deriving Show

newtype Unit = Unit [TopLevel] deriving Show

data TopLevel = Signature Identifier () {- TODO -} | Definition Identifier [Pattern] Expression deriving Show

data Type

data Pattern
    = Ref Identifier
    | LiteralPat Literal
    deriving Show

data Expression
    = Application Expression [Expression]
    | Var Identifier
    | Let [(Pattern, Expression)] Expression
    | If Expression Expression Expression
    | Lambda (NonEmpty Pattern) Expression
    | LiteralExpr Literal
    deriving Show

data Literal
    = IntLiteral Integer
    | FloatLiteral Double
    | CharLiteral Char
    | StringLiteral String
    deriving Show
