module Core
  ( Ann(..)
  , Env
  , ErrTipe(..)
  , Expr(..)
  , ExprAnn(..)
  , ExprTipe(..)
  , Eval
  , EvalErr(..)
  , EvalState(..)
  , EvalT(..)
  , getEnv
  , isFalse
  , isTrue
  , mkFalse
  , mkTrue
  , Result
  , ResultWithEnv
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
import Data.List as L
import Data.Map as M
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(..))

type Eval m res = EvalT m Env res

newtype EvalT m env res = EvalT (ExceptT EvalErr (StateT (EvalState env) m) res)

derive instance newtypeEvalT :: Newtype (EvalT m e r) _
derive newtype instance functorEvalT :: Functor m => Functor (EvalT m e)
derive newtype instance applyEvalT :: Monad m => Apply (EvalT m e)
derive newtype instance applicativeEvalT :: Monad m => Applicative (EvalT m e)
derive newtype instance bindEvalT :: Monad m => Bind (EvalT m e)
derive newtype instance monadParserT :: Monad m => Monad (EvalT m e)
derive newtype instance monadRecParserT :: MonadRec m => MonadRec (EvalT m e)
derive newtype instance monadStateParserT :: Monad m => MonadState (EvalState e) (EvalT m e)
derive newtype instance monadErrorParserT :: Monad m => MonadError EvalErr (EvalT m e)
derive newtype instance monadThrowParserT :: Monad m => MonadThrow EvalErr (EvalT m e)

type Result a = ResultWithEnv EvalErr a

type ResultWithEnv a b = { env :: Env, result :: Either a b }

runEval :: forall m a. Monad m => Env -> Eval m a -> m (Result a)
runEval env evaled = resultWithEnv <$> rn evaled evalState
  where
    evalState = EvalState { env: env }
    rn = runStateT <<< runExceptT <<< unwrap

resultWithEnv :: forall a. Tuple (Either EvalErr a) (EvalState Env) -> Result a
resultWithEnv (Tuple result (EvalState { env })) = { env, result }

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

throw :: forall m e a. Monad m => Ann -> ErrTipe -> EvalT m e a
throw ann name = throwError $ EvalErr name ann.srcSpan

data ErrTipe
  = NumArgs { expected :: Int, received :: Int }
  | WrongTipe { expected :: ExprTipe, received :: ExprTipe }
  | LstLength { expected :: Int, received :: Int }
  | UnknownVar { varName :: String }
  | EmptyFnApplication
  | UnknownErr
  | TrueCondClauseNotFound

newtype EvalState env = EvalState { env :: env }
derive instance evalStateNewtype :: Newtype (EvalState a) _

getEnv :: forall m e. Monad m => EvalT m e e
getEnv = S.get >>= (unwrap >>> _.env >>> pure)

updateEnv :: forall m. Monad m => String -> ExprAnn -> EvalT m Env Unit
updateEnv key val = do
  evalState <- unwrap <$> S.get
  S.put $ EvalState (evalState { env = M.insert key val evalState.env })

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

type Env = M.Map String ExprAnn

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
