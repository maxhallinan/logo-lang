module Parser (Parser, ParseErr, parseMany, parseOne) where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Either (Either)
import Data.List (List(..), manyRec)
import Data.String.CodeUnits (toCharArray)
import ExprAnn as E
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as C
import Text.Parsing.Parser.Language as L
import Text.Parsing.Parser.Pos (Position(..))
import Text.Parsing.Parser.String as S
import Text.Parsing.Parser.Token as T

type Parser a = P.Parser String a

type ParseErr = P.ParseError

parseMany :: String -> Either P.ParseError (List E.ExprAnn)
parseMany = flip P.runParser $ fileOf (manyOf exprAnn)

parseOne :: String -> Either P.ParseError E.ExprAnn
parseOne = flip P.runParser $ fileOf exprAnn

fileOf :: forall a. Parser a -> Parser a
fileOf = C.between lexer.whiteSpace S.eof

manyOf :: forall a. Parser a -> Parser (List a)
manyOf = flip C.sepBy lexer.whiteSpace

exprAnn :: Parser E.ExprAnn
exprAnn = fix $ \p -> number <|> symbol <|> quoted p <|> listOf p

listOf :: Parser E.ExprAnn -> Parser E.ExprAnn
listOf p = annotate $ E.Lst <$> lexer.parens (manyRec p)

quoted :: Parser E.ExprAnn -> Parser E.ExprAnn
quoted p = annotate $ do
  _ <- S.string "'"
  quote <- annotate $ pure (E.SFrm E.Quote)
  x <- p
  pure $ E.Lst (Cons quote (pure x))

number :: Parser E.ExprAnn
number = C.try float <|> integer

integer :: Parser E.ExprAnn
integer = annotate $ map E.Integer lexer.integer

float :: Parser E.ExprAnn
float = annotate $ map E.Float lexer.float

symbol :: Parser E.ExprAnn
symbol = annotate $ do
  identifier <- lexer.identifier
  case identifier of
    "cons" ->
      pure $ E.SFrm E.Cons
    "label" ->
      pure $ E.SFrm E.Def
    "equal?" ->
      pure $ E.SFrm E.IsEq
    "atom?" ->
      pure $ E.SFrm E.IsAtm
    "first" ->
      pure $ E.SFrm E.First
    "fn" ->
      pure $ E.SFrm E.Lambda
    "if" ->
      pure $ E.SFrm E.If
    "quote" ->
      pure $ E.SFrm E.Quote
    "rest" ->
      pure $ E.SFrm E.Rest
    _ ->
      pure $ E.Sym identifier

annotate :: Parser E.Expr -> Parser (E.ExprAnn)
annotate p = do
  begin <- location
  expr <- p
  end <- location
  let srcSpan = { begin: begin, end: end }
  let ann = { srcSpan: srcSpan }
  pure $ E.ExprAnn expr ann
  where location = P.position >>= (pure <<< unwrapPos)
        unwrapPos (Position loc) = loc

langDef :: T.LanguageDef
langDef = T.LanguageDef (T.unGenLanguageDef L.emptyDef)
  { commentLine = ";"
  , identLetter = identLetter
  , identStart = identStart
  }

lexer :: T.TokenParser
lexer = T.makeTokenParser langDef

identStart :: Parser Char
identStart = T.letter <|> oneOf "!$%&*/:<=>?~_^"

identLetter :: Parser Char
identLetter = identStart <|> T.digit <|> oneOf ".+-"

oneOf :: String -> Parser Char
oneOf = S.oneOf <<< toCharArray
