module Expr 
  ( Ann(..)
  , Env
  , Expr(..)
  , ExprAnn(..)
  , Location(..)
  , Position(..)
  , SFrm(..)
  ) where

import Prelude
import Data.Foldable (intercalate)
import Data.List (List)
import Data.Map (Map)
import Data.Newtype (class Newtype)

data ExprAnn = ExprAnn Expr Ann

instance showExprAnn :: Show ExprAnn where
  show (ExprAnn expr _) = show expr

newtype Ann = Ann { srcSpan :: Location }
derive instance newtypeAnn :: Newtype Ann _

newtype Location = SrcSpan { begin :: Position, end :: Position }
derive instance newtypeLocation :: Newtype Location _

newtype Position = SrcLoc { line :: Int, column :: Int }
derive instance newtypePosition :: Newtype Position _

data Expr 
  = Sym String
  | SFrm SFrm
  | Fn Env (List ExprAnn) ExprAnn
  | Lst (List ExprAnn)

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
