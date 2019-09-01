module Core
  ( Ann(..)
  , Env
  , Expr(..)
  , ExprAnn(..)
  , ExprTipe(..)
  , isFalse
  , isTrue
  , mkFalse
  , mkTrue
  , SrcLoc(..)
  , SrcSpan(..)
  , SFrm(..)
  , sfrmNumArgs
  , toExprTipe
  ) where

import Prelude

import Data.Foldable (intercalate)
import Data.List as L
import Data.Map (Map)

data ExprAnn = ExprAnn Expr Ann
derive instance exprAnnEq :: Eq ExprAnn

instance showExprAnn :: Show ExprAnn where
  show (ExprAnn expr _) = show expr

type Ann = { srcSpan :: SrcSpan }
type SrcSpan = { begin :: SrcLoc, end :: SrcLoc }
type SrcLoc = { line :: Int, column :: Int }

data Expr
  = Sym String
  | SFrm SFrm
  | Fn Env (L.List ExprAnn) ExprAnn
  | Lst (L.List ExprAnn)
  | Integer Int
  | Float Number
derive instance eqExpr :: Eq Expr

instance showExpr :: Show Expr where
  show (Sym name) = name
  show (SFrm sfrm) = show sfrm
  show (Fn _ _ _) = "<function>"
  show (Float n) = show n
  show (Integer n) = show n
  show (Lst exprs) = "(" <> exprs' <> ")"
    where exprs' = intercalate " " $ map show exprs

type Env = Map String ExprAnn

data SFrm
  = Car
  | Cdr
  | Cond
  | Conz
  | If
  | Def
  | IsAtm
  | IsEq
  | Lambda
  | Quote
derive instance eqSFrm :: Eq SFrm

instance showSFrm :: Show SFrm where
  show Car = "car"
  show Cdr = "cdr"
  show Cond = "cond"
  show Conz = "cons"
  show If = "if"
  show Def = "define"
  show IsAtm = "atom?"
  show IsEq = "equal?"
  show Lambda = "lambda"
  show Quote = "quote"

sfrmNumArgs :: SFrm -> Int
sfrmNumArgs Cond = 1
sfrmNumArgs Conz = 2
sfrmNumArgs Def = 1
sfrmNumArgs Car = 1
sfrmNumArgs If = 3
sfrmNumArgs IsAtm = 1
sfrmNumArgs IsEq = 1
sfrmNumArgs Lambda = 2
sfrmNumArgs Quote = 1
sfrmNumArgs Cdr = 1

data ExprTipe
  = SymTipe
  | LstTipe
  | FnTipe
  | SFrmTipe
  | FloatTipe
  | IntegerTipe

toExprTipe :: ExprAnn -> ExprTipe
toExprTipe (ExprAnn expr _ ) =
  case expr of
    Sym _ ->
      SymTipe
    Lst _ ->
      LstTipe
    Fn _ _ _ ->
      FnTipe
    SFrm _ ->
      SFrmTipe
    Float _ ->
      FloatTipe
    Integer _ ->
      IntegerTipe

mkTrue :: Ann -> ExprAnn
mkTrue ann = ExprAnn (Sym "true") ann

isTrue :: ExprAnn -> Boolean
isTrue (ExprAnn (Lst L.Nil) _) = false
isTrue (ExprAnn (Sym "false") _) = false
isTrue _ = true

mkFalse :: Ann -> ExprAnn
mkFalse ann = ExprAnn (Sym "false") ann

isFalse :: ExprAnn -> Boolean
isFalse = not <<< isTrue
