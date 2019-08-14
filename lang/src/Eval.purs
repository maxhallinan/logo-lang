module Eval
  ( ErrName(..)
  , Eval
  , EvalErr(..)
  , EvalT
  , ResultWithEnv
  , runMany
  , runOne
  ) where

import Prelude

import Control.Lazy (fix)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError)
import Control.Monad.Except.Trans (ExceptT, runExceptT, mapExceptT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State as S
import Control.Monad.State.Trans (StateT, runStateT, withStateT)
import Control.Monad.State.Class (class MonadState)
import Data.Either (Either(..), either)
import Data.Identity (Identity)
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (class Traversable, sequence, traverse)
import Data.Tuple (Tuple(..), fst)
import Data.Unit (unit)
import ExprAnn (Ann, Env, Expr(..), ExprAnn(..), SFrm(..), SrcSpan)

type Eval res = EvalT Identity Env res

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

data EvalErr = EvalErr ErrName SrcSpan

data ErrName
  = NumArgs
  | WrongTipe
  | LstLength
  | UnknownVar String
  | NotFn
  | Unknown
  | NotPair
  | NotImplemented

instance showEvalErr :: Show EvalErr where
  show (EvalErr errName _) =
    case errName of
      NumArgs ->
        "NumArgs"
      WrongTipe ->
        "WrongTipe"
      LstLength ->
        "LstLength"
      UnknownVar name ->
        "UnknownVar " <> name
      NotFn ->
        "NotFn"
      Unknown ->
        "Unknown"
      NotPair ->
        "NotPair"
      NotImplemented ->
        "NotImplemented"

throw :: forall m e a. Monad m => Ann -> ErrName -> EvalT m e a
throw ann name = throwError $ EvalErr name ann.srcSpan

newtype EvalState env = EvalState { env :: env }
derive instance evalStateNewtype :: Newtype (EvalState a) _

getEnv :: forall m e. Monad m => EvalT m e e
getEnv = S.get >>= (unwrap >>> _.env >>> pure)

updateEnv :: forall m a. Monad m => String -> ExprAnn -> EvalT m Env Unit
updateEnv key val = do
  evalState <- unwrap <$> S.get
  S.put $ EvalState (evalState { env = M.insert key val evalState.env })

type ResultWithEnv a = { env :: Env, result :: Either EvalErr a }

runMany :: Env -> L.List ExprAnn -> Identity (ResultWithEnv (L.List ExprAnn))
runMany env exprs = run env $ evalMany exprs

runOne :: Env -> ExprAnn -> Identity (ResultWithEnv ExprAnn)
runOne env expr = run env $ evalOne expr

run :: forall a. Env -> Eval a -> Identity (ResultWithEnv a)
run env evaled = resultWithEnv <$> rn evaled evalState
  where
    evalState = EvalState { env: env }
    rn = runStateT <<< runExceptT <<< unwrap

resultWithEnv :: forall a. Tuple (Either EvalErr a) (EvalState Env) -> ResultWithEnv a
resultWithEnv (Tuple result (EvalState { env })) = { env, result }

evalMany :: L.List ExprAnn -> Eval (L.List ExprAnn)
evalMany = sequence <<< map eval

evalOne :: ExprAnn -> Eval ExprAnn
evalOne = eval

eval :: ExprAnn -> Eval ExprAnn
eval (ExprAnn expr ann) =
  case expr of
    (Sym name) ->
      evalSym ann name
    (Lst (L.Cons (ExprAnn (SFrm sfrm) _) args)) ->
      evalSFrm ann sfrm args
    (Lst xs) ->
      evalLst ann xs
    _ ->
      throw ann NotImplemented

evalSym :: Ann -> String -> Eval ExprAnn
evalSym ann name = do
  env <- getEnv
  case M.lookup name env of
    Just expr ->
      pure expr
    Nothing ->
      throw ann (UnknownVar name)

type Args = L.List ExprAnn

evalSFrm :: Ann -> SFrm -> Args -> Eval ExprAnn
evalSFrm ann _ L.Nil = throw ann NumArgs
evalSFrm ann Cons args = evalCons ann args
evalSFrm ann Def args = evalDef ann args
evalSFrm ann First args = evalFirst ann args
evalSFrm ann If args = evalIf ann args
evalSFrm ann IsAtm args = evalIsAtm ann args
evalSFrm ann IsEq args = evalIsEq ann args
evalSFrm ann Lambda args = evalLambda ann args
evalSFrm ann Quote args = evalQuote ann args
evalSFrm ann Rest args = evalRest ann args
evalSFrm ann _ _ = throw ann NotImplemented

evalCons :: Ann -> Args -> Eval ExprAnn
evalCons ann (L.Cons e1 (L.Cons e2 L.Nil)) = do
  h <- eval e1
  t <- eval e2
  case t of
    (ExprAnn (Lst t') _) ->
      pure $ ExprAnn (Lst (L.Cons h t')) ann
    _ ->
      throw ann WrongTipe
evalCons ann _ = throw ann NumArgs

evalDef :: Ann -> Args -> Eval ExprAnn
evalDef ann (L.Cons sym (L.Cons e2 L.Nil)) = do
  val <- eval e2
  case sym of
    ExprAnn (Sym name) _ -> do
      _ <- updateEnv name val
      pure val
    _ ->
      throw ann WrongTipe
evalDef ann _ = throw ann NumArgs

evalFirst :: forall a. Ann -> Args -> Eval ExprAnn
evalFirst ann (L.Cons e L.Nil) = do
  ExprAnn xs _ <- eval e
  case xs of
    Lst L.Nil -> throw ann LstLength
    Lst (L.Cons h _) -> pure h
    _ -> throw ann WrongTipe
evalFirst ann _ = throw ann NumArgs

evalIf :: Ann -> Args -> Eval ExprAnn
evalIf ann (L.Cons p (L.Cons e1 (L.Cons e2 L.Nil))) = do
  ExprAnn p' _ <- eval p
  case p' of
    Lst L.Nil ->
      eval e2
    _ ->
      eval e1
evalIf ann _ = throw ann NumArgs

evalIsAtm :: Ann -> Args -> Eval ExprAnn
evalIsAtm ann (L.Cons e L.Nil) = do
  ExprAnn x _ <- eval e
  case x of
    Sym _ ->
      pure $ ExprAnn (Sym "true") ann
    Lst L.Nil ->
      pure $ ExprAnn (Sym "true") ann
    _ ->
      pure $ ExprAnn (Lst L.Nil) ann
evalIsAtm ann _ = throw ann NumArgs

evalIsEq :: Ann -> Args -> Eval ExprAnn
evalIsEq ann (L.Cons e1 (L.Cons e2 L.Nil)) = do
  ExprAnn x _ <- eval e1
  ExprAnn y _ <- eval e2
  case Tuple x y of
    Tuple (Sym name1) (Sym name2) ->
      if name1 == name2
      then pure $ ExprAnn (Sym "true") ann
      else pure $ ExprAnn (Lst L.Nil) ann
    Tuple (Lst L.Nil) (Lst L.Nil) ->
      pure $ ExprAnn (Sym "true") ann
    _ ->
      pure $ ExprAnn (Lst L.Nil) ann
evalIsEq ann _ = throw ann NumArgs

evalLambda :: Ann -> Args -> Eval ExprAnn
evalLambda ann (L.Cons params (L.Cons body L.Nil)) = do
  case params of
    ExprAnn (Lst params') _ -> do
      env <- getEnv
      pure $ ExprAnn (Fn env params' body) ann
    _ ->
      throw ann WrongTipe
evalLambda ann _ = throw ann NumArgs

evalQuote :: Ann -> Args -> Eval ExprAnn
evalQuote ann (L.Cons expr L.Nil) = pure $ expr
evalQuote ann _ = throw ann NumArgs

evalRest :: Ann -> Args -> Eval ExprAnn
evalRest ann (L.Cons e L.Nil) = do
  ExprAnn xs _ <- eval e
  case xs of
    Lst L.Nil ->
      pure $ ExprAnn (Lst L.Nil) ann
    Lst (L.Cons _ t) ->
      pure $ ExprAnn (Lst t) ann
    _ -> throw ann WrongTipe
evalRest ann _ = throw ann NumArgs

evalLst :: Ann -> Args -> Eval ExprAnn
evalLst ann (L.Cons x xs) = do
  ExprAnn fn _ <- eval x
  args <- traverse eval xs
  case fn of
    Fn localEnv params body -> do
      applyLambda ann params args body
    _ ->
      throw ann NotFn
evalLst ann L.Nil = throw ann NumArgs

applyLambda :: Ann -> L.List ExprAnn -> L.List ExprAnn -> ExprAnn -> Eval ExprAnn
applyLambda ann params args body = do
  env <- bindArgs ann params args
  let evalState = EvalState { env: env }
  EvalT $ mapExceptT (withStateT $ const evalState) (unwrap $ eval body)

bindArgs :: Ann -> L.List ExprAnn -> L.List ExprAnn -> Eval Env
bindArgs ann params args = do
  env <- getEnv
  bindings <- sequence (L.zipWith toBinding params args)
  pure $ M.fromFoldable bindings <> env
  where
    toBinding :: ExprAnn -> ExprAnn -> Eval (Tuple String ExprAnn)
    toBinding param arg = do
      paramName <- toParamName param
      pure $ Tuple paramName arg

    toParamName :: ExprAnn -> Eval String
    toParamName (ExprAnn param _) =
      case param of
        Sym name ->
          pure name
        _ ->
          throw ann Unknown
