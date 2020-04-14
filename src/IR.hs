module IR
    ( Rec
    , Identifier(..)
    , Unit(..)
    , TopLevel(..)
    , Type(..)
    , Pattern(..)
    , Expression(..)
    , Literal(..)
    ) where

import Data.List.NonEmpty (NonEmpty)

import qualified Data.Text as Text

type Rec = Bool

newtype Identifier = Identifier Text.Text deriving Show

newtype Unit = Unit [TopLevel] deriving Show

data TopLevel
    = Signature Identifier Type
    | Definition (NonEmpty (Rec, NonEmpty Pattern, Expression))
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
    | Let (NonEmpty (Rec, NonEmpty Pattern, Expression)) Expression
    | Match Expression (NonEmpty (Pattern, Expression))
    | LambdaMatch (NonEmpty (Pattern, Expression))
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
