module IR
  ( Identifier(..)
  , Unit(..)
  , TopLevel(..)
  , Type(..)
  , Pattern(..)
  , Expression(..)
  , Literal(..)
  ) where

import Data.List.NonEmpty (NonEmpty)

import qualified Data.Text as Text
import Data.Void (Void)

type Rec = Bool

newtype Identifier = Identifier Text.Text deriving Show

newtype Unit = Unit [TopLevel] deriving Show

data TopLevel
    = Signature Identifier Type
    | Definition Rec Identifier [Pattern] Expression
    | DataDefinition
        Type
        [(Type, Maybe Type)]  -- (constructor, signature)
    | TypeAlias Type Type
    deriving Show

data Type
    = TypeApp Type [Type]
    | Typename Identifier
    | RecordType
        [(Identifier, Type)]  -- fields
        (Maybe Identifier)  -- row variable
    deriving Show

data Pattern
    = Ref Identifier
    | LiteralPat Literal
    deriving Show

data Expression
    = Application Expression [Expression]
    | Var Identifier
    | Let (NonEmpty (Rec, Pattern, Expression)) Expression
    | If Expression Expression Expression
    | Lambda (NonEmpty Pattern) Expression
    | RecordLiteral
        [(Identifier, Expression)]  -- fields
        (Maybe Expression)  -- row expression
    | LiteralExpr Literal
    deriving Show

data Literal
    = IntLiteral Integer
    | FloatLiteral Double
    | CharLiteral Char
    | StringLiteral String
    deriving Show
