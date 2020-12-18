{-# LANGUAGE OverloadedStrings #-}

module Parser
    ( parseUnit
    ) where

import Control.Applicative (liftA2)
import Data.Char (isAsciiLower, isAsciiUpper)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (isJust)
import Data.Void (Void)

import qualified Data.HashSet as Set
import qualified Data.Text as Text
import Text.Megaparsec hiding (ParseError)
import Text.Megaparsec.Char

import IR

-- combinators

type Parser = Parsec Void Text.Text
type ParseError = ParseErrorBundle Text.Text Void

-- maybe we should check if String or Text is faster for HashSet?
keywordIdentifiers :: Set.HashSet Text.Text
keywordIdentifiers = Set.fromList ["data", "type", "sig", "let", "rec", "and", "in", "match", "with", "if", "then", "else"]

keywordOperators :: Set.HashSet Text.Text
keywordOperators = Set.fromList ["=", ":", "\\", "=>", "|"]

-- ! # $ % & * + - / : < = > ? \ ^ | ~
isSymbol :: Char -> Bool
isSymbol c = c == '!'
    || c >= '#' && c <= '&'
    || c == '*'
    || c == '+'
    || c == '-'
    || c == '/'
    || c == ':'
    || c >= '<' && c <= '?'
    || c == '\\'
    || c == '^'
    || c == '|'
    || c == '~'

isLowercase :: Char -> Bool
isLowercase c = isAsciiLower c || c == '_'

isUppercase :: Char -> Bool
isUppercase = isAsciiUpper

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

isAlphanum :: Char -> Bool
isAlphanum c = isLowercase c || isUppercase c || isDigit c

-- TODO
typename :: Parser Identifier
typename = lexeme $ do
    name <- liftA2 Text.cons (satisfy isUppercase) (takeWhileP Nothing isAlphanum)
    pure $ Identifier name

identifier :: Parser Identifier
identifier = lexeme ident <?> "identifier"
  where
    ident = do
        name <- liftA2 Text.cons (satisfy isLowercase) (takeWhileP Nothing isAlphanum)
        if Set.member name keywordIdentifiers
            then fail "unexpected keyword"
            else pure $ Identifier name

operator :: Parser Identifier
operator = lexeme $ do
    name <- takeWhile1P Nothing isSymbol
    if Set.member name keywordOperators
        then fail "unexpected keyword"
        else pure $ Identifier name

-- TODO: add support for operators
keyword :: Text.Text -> Parser ()
keyword key
    | key `elem` keywordIdentifiers = lexeme (string key *> notFollowedBy (satisfy isAlphanum)) <?> "keyword `" ++ Text.unpack key ++ "`"
    | key `elem` keywordOperators = lexeme (string key *> notFollowedBy (satisfy isSymbol)) <?> "keyword `" ++ Text.unpack key ++ "`"
    | otherwise =  error $ "Parser.Combinators.keyword: expected keyword, got '" ++ Text.unpack key ++ "'"

-- parses additional trailing whitespaces
lexeme :: Parser a -> Parser a
lexeme = (<* space)

punct :: Text.Text -> Parser ()
punct = (*> pure ()) . lexeme . string


parseUnit :: String -> Text.Text -> Either ParseError Unit
parseUnit = parse (space *> unit <* eof)

unit :: Parser Unit
unit = Unit <$> many (dataDefinition <|> typeAlias <|> letDefinition <|> signature')

dataDefinition :: Parser TopLevel
dataDefinition = do
    keyword "data"
    ty <- typeExpression
    constructors <- many singleData
    pure $ DataDefinition ty constructors
  where
    singleData = keyword "|" *> liftA2 (,) typeExpression (optional $ keyword ":" *> typeSignature)

typeAlias :: Parser TopLevel
typeAlias = do
    keyword "type"
    alias <- typeExpression
    keyword "="
    ty <- typeSignature
    pure $ TypeAlias alias ty

-- top level let bindings
letDefinition :: Parser TopLevel
letDefinition = do
    keyword "let"
    isRec <- isJust <$> optional (try (keyword "rec"))
    args <- some pattern'
    keyword "="
    expr <- expression
    andBindings <- many letAnd
    pure $ Definition ((isRec, NonEmpty.fromList args, expr) :| andBindings)

signature' :: Parser TopLevel
signature' = do
    keyword "sig"
    name <- identifier
    keyword ":"
    ty <- typeSignature
    pure $ Signature name ty

typeExpression :: Parser Type
typeExpression = do
    ls <- some singleType
    pure $ case ls of
        [x] -> x
        (x : xs) -> TypeApp x xs
        _ -> error "unreachable"
  where
    singleType = recordType
        <|> try (fmap Typename typename)
        <?> "type"

recordType :: Parser Type
recordType = do
    punct "{"
    fields <- field `sepEndBy` punct ","
    row <- optional $ keyword "|" *> identifier
    punct "}"
    pure $ RecordType fields row
  where
    field = do
        name <- identifier
        keyword ":"
        ty <- typeSignature
        pure (name, ty)

-- type expression, used in
typeSignature :: Parser Type
typeSignature = do
    ls <- some singleType
    pure $ case ls of
        [x] -> x
        (x : xs) -> TypeApp x xs
        _ -> error "unreachable"
  where
    singleType = punct "(" *> typeSignature <* punct ")"
        <|> recordType
        <|> try (fmap Typename typename)
        <?> "type"

-- TODO
pattern' :: Parser Pattern
pattern' = Ref <$> identifier

expression :: Parser Expression
expression = do
    ls <- some singleExpr
    pure $ case ls of
        [x] -> x
        (x : xs) -> Application x xs
        _ -> error "unreachable"
  where
    singleExpr = punct "(" *> expression <* punct ")"
        <|> lambda
        <|> record
        <|> fmap LiteralExpr literal
        <|> letExpression
        <|> matchExpression
        <|> ifExpression
        <|> try (fmap Var identifier)
        <?> "expression"

lambda :: Parser Expression
lambda = do
    try $ keyword "\\"
    pats <- NonEmpty.fromList <$> some pattern'
    keyword "=>"
    expr <- expression
    pure $ Lambda pats expr

record :: Parser Expression
record = do
    punct "{"
    fields <- field `sepEndBy` punct ","
    row <- optional $ keyword "|" *> expression
    punct "}"
    pure $ RecordLiteral fields row
  where
    field = do
        name <- identifier
        keyword "="
        expr <- expression
        pure (name, expr)

letExpression :: Parser Expression
letExpression = do
    try $ keyword "let"
    isRec <- isJust <$> (optional $ try (keyword "rec"))
    args <- some pattern'
    keyword "="
    expr <- expression
    andBindings <- many letAnd
    keyword "in"
    inExpr <- expression
    pure $ Let ((isRec, NonEmpty.fromList args, expr) :| andBindings) inExpr

letAnd :: Parser (Rec, NonEmpty Pattern, Expression)
letAnd = do
    keyword "and"
    isRec <- isJust <$> optional (try (keyword "rec"))
    args <- some pattern'
    keyword "="
    expr <- expression
    pure (isRec, NonEmpty.fromList args, expr)

matchExpression :: Parser Expression
matchExpression = do
    keyword "match"
    lookAhead (keyword "|") *> (LambdaMatch <$> arms) <|> do
        expr <- expression
        keyword "with"
        Match expr <$> arms
  where
    arms = fmap NonEmpty.fromList . some $ do
        keyword "|"
        pat <- pattern'
        keyword "=>"
        expr <- expression
        pure (pat, expr)

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
literal = lexeme $ IntLiteral . read <$> some (satisfy isDigit)  -- TODO: use takeWhile1P
    <|> CharLiteral <$> (punct "'" *> noneOf ['"'] <* punct "'")  -- TODO: escape chars
    <|> StringLiteral <$> (punct "\"" *> many (noneOf ['"']) <* punct "\"")  -- TODO: escape chars
