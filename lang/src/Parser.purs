module Parser where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Either (Either)
import Data.List (List(..), manyRec)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Expr as E
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as C
import Text.Parsing.Parser.Language as L
import Text.Parsing.Parser.Pos (Position(..))
import Text.Parsing.Parser.String as S
import Text.Parsing.Parser.Token as T

type Parser a = P.Parser String a

parseStr :: forall a. String -> Either P.ParseError (List E.ExprAnn)
parseStr = flip P.runParser program

program :: forall a. Parser (List E.ExprAnn)
program = C.between lexer.whiteSpace S.eof exprAnns
  where exprAnns = C.sepBy exprAnn lexer.whiteSpace

exprAnn :: forall a. Parser E.ExprAnn
exprAnn = fix $ \p -> symbol <|> quoted p <|> listOf p

listOf :: forall a. Parser E.ExprAnn -> Parser E.ExprAnn
listOf p = annotate $ E.Lst <$> lexer.parens (manyRec p)

quoted :: forall a. Parser E.ExprAnn -> Parser E.ExprAnn
quoted p = annotate $ do
  _ <- S.string "'"
  quote <- annotate $ pure (E.SFrm E.Quote)
  x <- p
  pure $ E.Lst (Cons quote (pure x))

symbol :: forall a. Parser E.ExprAnn
symbol = annotate $ do
  identifier <- lexer.identifier
  case identifier of
    "::" ->
      pure $ E.SFrm E.Cons
    "=" ->
      pure $ E.SFrm E.Def
    "==" ->
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
  let srcSpan = E.SrcSpan { begin: E.SrcLoc begin, end: E.SrcLoc end }
  let ann = E.Ann { srcSpan: srcSpan }
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
