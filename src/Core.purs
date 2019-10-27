module Core
  ( Ann(..)
  , Bindings(..)
  , Env
  , ErrTipe(..)
  , Expr(..)
  , ExprAnn(..)
  , ExprTipe(..)
  , Eval
  , EvalErr(..)
  , EvalState(..)
  , EvalT(..)
  , PrimFns(..)
  , getEnv
  , isFalse
  , isTrue
  , mkFalse
  , mkTrue
  , runEval
  , SrcLoc(..)
  , SrcSpan(..)
  , SFrm(..)
  , sfrmNumArgs
  , throw
  , toExprTipe
  , updateEnv
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State as S
import Control.Monad.State.Trans (StateT, runStateT)
import Control.Monad.State.Class (class MonadState)
import Data.Either (Either)
import Data.Foldable (intercalate)
import Data.List (List)
import Data.List as L
import Data.Map (Map)
import Data.Map as M
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(..))

type Eval m = EvalT (Bindings (PrimFns m)) m

newtype EvalT bindings m a = EvalT (ExceptT EvalErr (StateT (EvalState bindings) m) a)

derive instance newtypeEvalT :: Newtype (EvalT r m a) _
derive newtype instance functorEvalT :: Functor m => Functor (EvalT r m)
derive newtype instance applyEvalT :: Monad m => Apply (EvalT r m)
derive newtype instance applicativeEvalT :: Monad m => Applicative (EvalT r m)
derive newtype instance bindEvalT :: Monad m => Bind (EvalT r m)
derive newtype instance monadEvalT :: Monad m => Monad (EvalT r m)
derive newtype instance monadRecEvalT :: MonadRec m => MonadRec (EvalT r m)
derive newtype instance monadStateEvalT :: Monad m => MonadState (EvalState r) (EvalT r m)
derive newtype instance monadErrorEvalT :: Monad m => MonadError EvalErr (EvalT r m)
derive newtype instance monadThrowEvalT :: Monad m => MonadThrow EvalErr (EvalT r m)

runEval
  :: forall m a
   . Monad m
  => Bindings (PrimFns m)
  -> Eval m a
  -> m (Tuple (Either EvalErr a) (Bindings (PrimFns m)))
runEval bindings evaled = resultWithEnv <$> rn evaled evalState
  where
    evalState = EvalState { bindings }
    rn = runStateT <<< runExceptT <<< unwrap

resultWithEnv
  :: forall a bindings
   . Tuple a (EvalState bindings)
  -> Tuple a bindings
resultWithEnv (Tuple x (EvalState { bindings })) = Tuple x bindings

data EvalErr = EvalErr ErrTipe SrcSpan

instance showEvalErr :: Show EvalErr where
  show (EvalErr errTipe _) =
    case errTipe of
      NumArgs _ ->
        "wrong number of arguments"
      WrongTipe _ ->
        "wrong type"
      LstLength ctx ->
        "wrong list length " <> (show $ ctx.received)
      UnknownVar { varName } ->
        "unknown identifier " <> varName
      EmptyFnApplication ->
        "empty list in function application position"
      UnknownErr ->
        "unknown error"
      TrueCondClauseNotFound ->
        "cond found no true predicates. Add an `else` clause at the end."

throw :: forall r m a. Monad m => Ann -> ErrTipe -> EvalT r m a
throw ann name = throwError $ EvalErr name ann.srcSpan

data ErrTipe
  = NumArgs { expected :: Int, received :: Int }
  | WrongTipe { expected :: ExprTipe, received :: ExprTipe }
  | LstLength { expected :: Int, received :: Int }
  | UnknownVar { varName :: String }
  | EmptyFnApplication
  | UnknownErr
  | TrueCondClauseNotFound

newtype EvalState bindings = EvalState { bindings :: bindings }

derive instance evalStateNewtype :: Newtype (EvalState a) _

getEnv :: forall r m. Monad m => EvalT (Bindings r) m Env
getEnv = S.get >>= (unwrap >>> _.bindings >>> unwrap >>> _.env >>> pure)

updateEnv :: forall m. Monad m => String -> ExprAnn -> EvalT (Bindings (PrimFns m)) m Unit
updateEnv key val =
  S.modify_ \(EvalState s@({ bindings: Bindings b })) ->
    EvalState $ s { bindings = Bindings b { env = M.insert key val b.env } }

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

newtype Bindings primFns = Bindings
  { env :: Env
  , primFns :: primFns
  }

derive instance newtypeBindings :: Newtype (Bindings a) _

derive newtype instance semigroupBindings :: Semigroup a => Semigroup (Bindings a)

derive newtype instance monoidBindings :: Monoid a => Monoid (Bindings a)

newtype PrimFns m = PrimFns (Map String (forall a. List ExprAnn -> Eval m a))

instance semigroupPrimFns :: Semigroup (PrimFns m) where
  append (PrimFns p1) (PrimFns p2) = PrimFns (p1 <> p2)

instance monoidPrimFns :: Monoid (PrimFns m) where
  mempty = PrimFns mempty

data SFrm
  = Car
  | Cdr
  | Cond
  | Conz
  | Def
  | Do
  | If
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
  show Do = "do"
  show IsAtm = "atom?"
  show IsEq = "equal?"
  show Lambda = "lambda"
  show Quote = "quote"

sfrmNumArgs :: SFrm -> Int
sfrmNumArgs Cond = 1
sfrmNumArgs Conz = 2
sfrmNumArgs Def = 1
sfrmNumArgs Do = 1
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
