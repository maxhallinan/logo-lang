module Lang.Parser (Parser, ParseErr, parseMany, parseOne) where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Lang.Core (Expr(..), ExprAnn(..), SFrm(..))
import Data.Either (Either)
import Data.List (List(..), manyRec)
import Data.String.CodeUnits (toCharArray)
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators  as C
import Text.Parsing.Parser.Language as L
import Text.Parsing.Parser.Pos (Position(..))
import Text.Parsing.Parser.String as S
import Text.Parsing.Parser.Token as T

type Parser a = P.Parser String a

type ParseErr = P.ParseError

parseMany :: String -> Either P.ParseError (List ExprAnn)
parseMany = flip P.runParser $ fileOf (manyOf exprAnn)

parseOne :: String -> Either P.ParseError ExprAnn
parseOne = flip P.runParser $ fileOf exprAnn

fileOf :: forall a. Parser a -> Parser a
fileOf = C.between lexer.whiteSpace S.eof

manyOf :: forall a. Parser a -> Parser (List a)
manyOf = flip C.sepBy lexer.whiteSpace

exprAnn :: Parser ExprAnn
exprAnn = fix $ \p -> number <|> symbol <|> quoted p <|> listOf p

listOf :: Parser ExprAnn -> Parser ExprAnn
listOf p = annotate $ Lst <$> lexer.parens (manyRec p)

quoted :: Parser ExprAnn -> Parser ExprAnn
quoted p = annotate $ do
  _ <- S.string "'"
  quote <- annotate $ pure (SFrm Quote)
  x <- p
  pure $ Lst (Cons quote (pure x))

number :: Parser ExprAnn
number = C.try float <|> integer

integer :: Parser ExprAnn
integer = annotate $ map Integer lexer.integer

float :: Parser ExprAnn
float = annotate $ map Float lexer.float

symbol :: Parser ExprAnn
symbol = annotate $ do
  identifier <- lexer.identifier
  case identifier of
    "car" ->
      pure $ SFrm Car
    "cdr" ->
      pure $ SFrm Cdr
    "cond" ->
      pure $ SFrm Cond
    "cons" ->
      pure $ SFrm Conz
    "define" ->
      pure $ SFrm Def
    "do" ->
      pure $ SFrm Do
    "equal?" ->
      pure $ SFrm IsEq
    "atom?" ->
      pure $ SFrm IsAtm
    "lambda" ->
      pure $ SFrm Lambda
    "if" ->
      pure $ SFrm If
    "pause" ->
      pure $ SFrm Pause
    "quote" ->
      pure $ SFrm Quote
    _ ->
      pure $ Sym identifier

annotate :: Parser Expr -> Parser ExprAnn
annotate p = do
  begin <- location
  expr <- p
  end <- location
  let srcSpan = { begin: begin, end: end }
  let ann = { srcSpan: srcSpan }
  pure $ ExprAnn expr ann
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
