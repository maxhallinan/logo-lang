module ExprAnn
  ( Ann(..)
  , Env
  , Expr(..)
  , ExprAnn(..)
  , SrcLoc(..)
  , SrcSpan(..)
  , SFrm(..)
  ) where

import Prelude
import Data.Foldable (intercalate)
import Data.List (List)
import Data.Map (Map)
import Data.Newtype (class Newtype)

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
  | Fn Env (List ExprAnn) ExprAnn
  | Lst (List ExprAnn)
derive instance eqExpr :: Eq Expr

instance showExpr :: Show Expr where
  show (Sym name) = name
  show (SFrm sfrm) = show sfrm
  show (Fn _ _ _) = "<function>"
  show (Lst exprs) = "(" <> exprs' <> ")"
    where exprs' = intercalate " " $ map show exprs

type Env = Map String ExprAnn

data SFrm
  = First
  | Rest
  | Cons
  | If
  | Def
  | IsAtm
  | IsEq
  | Lambda
  | Quote
derive instance eqSFrm :: Eq SFrm

instance showSFrm :: Show SFrm where
  show First = "first"
  show Rest = "rest"
  show Cons = "::"
  show If = "if"
  show Def = "def"
  show IsAtm = "atom?"
  show IsEq = "=="
  show Lambda = "fn"
  show Quote = "quote"
