module ExprAnn
  ( Ann(..)
  , Env
  , Expr(..)
  , ExprAnn(..)
  , ExprTipe(..)
  , boolOp1
  , boolOp2
  , isFalse
  , isTrue
  , mkFalse
  , mkTrue
  , Op1(..)
  , Op2(..)
  , primOp1
  , primOp2
  , Src(..)
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

type Ann = { src :: Src }

data Src = Primitive | Src SrcSpan
derive instance eqSrc :: Eq Src

type SrcSpan = { begin :: SrcLoc, end :: SrcLoc }
type SrcLoc = { line :: Int, column :: Int }

data Expr
  = Sym String
  | SFrm SFrm
  | Fn Env (L.List ExprAnn) ExprAnn
  | Lst (L.List ExprAnn)
  | Integer Int
  | Float Number
  | Op1 Op1 (Ann -> ExprAnn -> ExprAnn)
  | Op2 Op2 (Ann -> ExprAnn -> ExprAnn -> ExprAnn)

instance eqExpr :: Eq Expr where
  eq (Sym x) (Sym y) = eq x y
  eq (SFrm sfrm1) (SFrm sfrm2) = eq sfrm1 sfrm2
  eq (Fn e1 p1 b1) (Fn e2 p2 b2) = isEnvEq && isParamsEq && isBodyEq
    where
      isEnvEq = eq e1 e2
      isParamsEq = eq p1 p2
      isBodyEq = eq b1 b2
  eq (Lst xs) (Lst ys) = eq xs ys
  eq (Integer x) (Integer y) = eq x y
  eq (Float x) (Float y) = eq x y
  eq _ _ = false

instance showExpr :: Show Expr where
  show (Sym name) = name
  show (SFrm sfrm) = show sfrm
  show (Fn _ _ _) = "<function>"
  show (Float n) = show n
  show (Integer n) = show n
  show (Lst exprs) = "(" <> exprs' <> ")"
    where exprs' = intercalate " " $ map show exprs
  show (Op1 name _) = show name
  show (Op2 name _) = show name

type Env = Map String ExprAnn

data SFrm
  = Car
  | Cdr
  | Cond
  | Cons
  | If
  | Def
  | IsAtm
  | IsEq
  | Lambda
  | Quote
derive instance eqSFrm :: Eq SFrm

data Op1 = OpNot
derive instance eqOp1 :: Eq Op1

instance showOp1 :: Show Op1 where
  show OpNot = "not"

data Op2 = OpAnd | OpOr
derive instance eqOp2 :: Eq Op2

instance showOp2 :: Show Op2 where
  show OpAnd = "and"
  show OpOr = "or"

instance showSFrm :: Show SFrm where
  show Car = "car"
  show Cdr = "cdr"
  show Cond = "cond"
  show Cons = "cons"
  show If = "if"
  show Def = "define"
  show IsAtm = "atom?"
  show IsEq = "equal?"
  show Lambda = "lambda"
  show Quote = "quote"

sfrmNumArgs :: SFrm -> Int
sfrmNumArgs Cond = 1
sfrmNumArgs Cons = 2
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
  | Op1Tipe
  | Op2Tipe

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
    Op1 _ _ ->
      Op1Tipe
    Op2 _ _ ->
      Op2Tipe

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

primOp1 :: Op1 -> (Ann -> ExprAnn -> ExprAnn) -> ExprAnn
primOp1 name op = ExprAnn (Op1 name op) ann
  where ann = { src: Primitive }

primOp2 :: Op2 -> (Ann -> ExprAnn -> ExprAnn -> ExprAnn) -> ExprAnn
primOp2 name op = ExprAnn (Op2 name op) ann
  where ann = { src: Primitive }

boolOp1 :: (Boolean -> Boolean) -> Ann -> ExprAnn -> ExprAnn
boolOp1 op ann expr =
  if op (isTrue expr)
  then mkTrue ann
  else mkFalse ann

boolOp2 :: (Boolean -> Boolean -> Boolean) -> Ann -> ExprAnn -> ExprAnn -> ExprAnn
boolOp2 op ann e1 e2 =
  if op (isTrue e1) (isTrue e2)
  then mkTrue ann
  else mkFalse ann
