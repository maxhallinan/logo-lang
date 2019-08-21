module Eval
  ( ErrTipe(..)
  , Eval
  , EvalErr(..)
  , EvalT
  , Result
  , ResultWithEnv
  , evalMany
  , evalOne
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError)
import Control.Monad.Except.Trans (ExceptT, runExceptT, mapExceptT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State as S
import Control.Monad.State.Trans (StateT, runStateT, withStateT)
import Control.Monad.State.Class (class MonadState)
import Data.Either (Either)
import Data.Foldable (length)
import Data.Identity (Identity)
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import ExprAnn (Ann, Env, Expr(..), ExprAnn(..), ExprTipe(..), SFrm(..), SrcSpan, sfrmNumArgs, toExprTipe)

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

data EvalErr = EvalErr ErrTipe SrcSpan

data ErrTipe
  = NumArgs { expected :: Int, received :: Int }
  | WrongTipe { expected :: ExprTipe, received :: ExprTipe }
  | LstLength { expected :: Int, received :: Int }
  | UnknownVar { varName :: String }
  | EmptyFnApplication
  | UnknownErr

numArgsErr :: Int -> Int -> ErrTipe
numArgsErr expected received = NumArgs { expected, received }

throwNumArgsErr
  :: forall m e a
   . Monad m
  => Ann
  -> Int
  -> Int
  -> EvalT m e a
throwNumArgsErr ann expected received =
  throw ann $ numArgsErr expected received

throwSFrmNumArgsErr
  :: forall m e a
   . Monad m
  => Ann
  -> SFrm
  -> Args
  -> EvalT m e a
throwSFrmNumArgsErr ann sfrm args = throwNumArgsErr ann expected received
  where expected = sfrmNumArgs sfrm
        received = length args

wrongTipeErr :: ExprTipe -> ExprTipe -> ErrTipe
wrongTipeErr expected received = WrongTipe { expected, received }

throwWrongTipeErr
  :: forall m e a
   . Monad m
  => Ann
  -> ExprTipe
  -> ExprTipe
  -> EvalT m e a
throwWrongTipeErr ann expected received =
  throw ann $ wrongTipeErr expected received

lstLengthErr :: Int -> Int -> ErrTipe
lstLengthErr expected received = LstLength { expected, received }

throwLstLengthErr
  :: forall m e a
   . Monad m
  => Ann
  -> Int
  -> Int
  -> EvalT m e a
throwLstLengthErr ann expected received =
  throw ann $ lstLengthErr expected received

unknownVarErr :: String -> ErrTipe
unknownVarErr varName = UnknownVar { varName }

throwUnknownVarErr
  :: forall m e a
   . Monad m
  => Ann
  -> String
  -> EvalT m e a
throwUnknownVarErr ann varName =
  throw ann $ unknownVarErr varName

unknownErr :: ErrTipe
unknownErr = UnknownErr

throwUnknownErr
  :: forall m e a
   . Monad m
  => Ann
  -> EvalT m e a
throwUnknownErr ann = throw ann unknownErr

emptyFnApplicationErr :: ErrTipe
emptyFnApplicationErr = EmptyFnApplication

throwEmptyFnApplicationErr
  :: forall m e a
   . Monad m
  => Ann
  -> EvalT m e a
throwEmptyFnApplicationErr ann = throw ann emptyFnApplicationErr

instance showEvalErr :: Show EvalErr where
  show (EvalErr errTipe _) =
    case errTipe of
      NumArgs _ ->
        "wrong number of arguments"
      WrongTipe _ ->
        "wrong type"
      LstLength _ ->
        "wrong list length"
      UnknownVar { varName } ->
        "unknown identifier " <> varName
      EmptyFnApplication ->
        "empty list in function application position"
      UnknownErr ->
        "unknown error"

throw :: forall m e a. Monad m => Ann -> ErrTipe -> EvalT m e a
throw ann name = throwError $ EvalErr name ann.srcSpan

newtype EvalState env = EvalState { env :: env }
derive instance evalStateNewtype :: Newtype (EvalState a) _

getEnv :: forall m e. Monad m => EvalT m e e
getEnv = S.get >>= (unwrap >>> _.env >>> pure)

updateEnv :: forall m. Monad m => String -> ExprAnn -> EvalT m Env Unit
updateEnv key val = do
  evalState <- unwrap <$> S.get
  S.put $ EvalState (evalState { env = M.insert key val evalState.env })

type Result a = ResultWithEnv EvalErr a

type ResultWithEnv a b = { env :: Env, result :: Either a b }

evalMany :: Env -> L.List ExprAnn -> Identity (Result (L.List ExprAnn))
evalMany env = runEval env <<< sequence <<< map eval

evalOne :: Env -> ExprAnn -> Identity (Result ExprAnn)
evalOne env = runEval env <<< eval

runEval :: forall a. Env -> Eval a -> Identity (Result a)
runEval env evaled = resultWithEnv <$> rn evaled evalState
  where
    evalState = EvalState { env: env }
    rn = runStateT <<< runExceptT <<< unwrap

resultWithEnv :: forall a. Tuple (Either EvalErr a) (EvalState Env) -> Result a
resultWithEnv (Tuple result (EvalState { env })) = { env, result }

eval :: ExprAnn -> Eval ExprAnn
eval exprAnn@(ExprAnn expr ann) =
  case expr of
    (Sym name) ->
      evalSym ann name
    (Lst (L.Cons (ExprAnn (SFrm sfrm) _) args)) ->
      evalSFrm ann sfrm args
    (Lst xs) ->
      evalLst ann xs
    (Float _) ->
      pure $ exprAnn
    (Integer _) ->
      pure $ exprAnn
    _ ->
      throwUnknownErr ann

evalSym :: Ann -> String -> Eval ExprAnn
evalSym ann name = do
  env <- getEnv
  case M.lookup name env of
    Just expr ->
      pure expr
    Nothing ->
      throwUnknownVarErr ann name

type Args = L.List ExprAnn

evalSFrm :: Ann -> SFrm -> Args -> Eval ExprAnn
evalSFrm ann sfrm L.Nil = throwSFrmNumArgsErr ann sfrm L.Nil
evalSFrm ann Cons args = evalCons ann args
evalSFrm ann Def args = evalDef ann args
evalSFrm ann First args = evalFirst ann args
evalSFrm ann If args = evalIf ann args
evalSFrm ann IsAtm args = evalIsAtm ann args
evalSFrm ann IsEq args = evalIsEq ann args
evalSFrm ann Lambda args = evalLambda ann args
evalSFrm ann Quote args = evalQuote ann args
evalSFrm ann Rest args = evalRest ann args

evalCons :: Ann -> Args -> Eval ExprAnn
evalCons ann (L.Cons e1 (L.Cons e2 L.Nil)) = do
  h <- eval e1
  t <- eval e2
  case t of
    (ExprAnn (Lst t') _) ->
      pure $ ExprAnn (Lst (L.Cons h t')) ann
    _ ->
      throwWrongTipeErr ann LstTipe (toExprTipe t)
evalCons ann args = throwSFrmNumArgsErr ann Cons args

evalDef :: Ann -> Args -> Eval ExprAnn
evalDef ann (L.Cons e1 (L.Cons e2 L.Nil)) = do
  val <- eval e2
  case e1 of
    ExprAnn (Sym name) _ -> do
      _ <- updateEnv name val
      pure val
    _ ->
      throwWrongTipeErr ann SymTipe (toExprTipe e1)
evalDef ann args = throwSFrmNumArgsErr ann Def args

evalFirst :: Ann -> Args -> Eval ExprAnn
evalFirst ann (L.Cons e L.Nil) = do
  expr@(ExprAnn xs _) <- eval e
  case xs of
    Lst L.Nil ->
      throwLstLengthErr ann 1 0
    Lst (L.Cons h _) ->
      pure h
    _ ->
      throwWrongTipeErr ann LstTipe (toExprTipe expr)
evalFirst ann args = throwSFrmNumArgsErr ann First args

evalIf :: Ann -> Args -> Eval ExprAnn
evalIf ann (L.Cons p (L.Cons e1 (L.Cons e2 L.Nil))) = do
  ExprAnn p' _ <- eval p
  case p' of
    Lst L.Nil ->
      eval e2
    _ ->
      eval e1
evalIf ann args = throwSFrmNumArgsErr ann If args

evalIsAtm :: Ann -> Args -> Eval ExprAnn
evalIsAtm ann (L.Cons e L.Nil) = do
  ExprAnn x _ <- eval e
  case x of
    Float _ ->
      pure $ ExprAnn (Sym "true") ann
    Integer _ ->
      pure $ ExprAnn (Sym "true") ann
    Sym _ ->
      pure $ ExprAnn (Sym "true") ann
    Lst L.Nil ->
      pure $ ExprAnn (Sym "true") ann
    _ ->
      pure $ ExprAnn (Lst L.Nil) ann
evalIsAtm ann args = throwSFrmNumArgsErr ann IsAtm args

evalIsEq :: Ann -> Args -> Eval ExprAnn
evalIsEq ann (L.Cons e1 (L.Cons e2 L.Nil)) = do
  ExprAnn x _ <- eval e1
  ExprAnn y _ <- eval e2
  case Tuple x y of
    Tuple (Float n1) (Float n2) ->
      if n1 == n2
      then pure $ ExprAnn (Sym "true") ann
      else pure $ ExprAnn (Lst L.Nil) ann
    Tuple (Integer n1) (Integer n2) ->
      if n1 == n2
      then pure $ ExprAnn (Sym "true") ann
      else pure $ ExprAnn (Lst L.Nil) ann
    Tuple (Sym name1) (Sym name2) ->
      if name1 == name2
      then pure $ ExprAnn (Sym "true") ann
      else pure $ ExprAnn (Lst L.Nil) ann
    Tuple (Lst L.Nil) (Lst L.Nil) ->
      pure $ ExprAnn (Sym "true") ann
    _ ->
      pure $ ExprAnn (Lst L.Nil) ann
evalIsEq ann args = throwSFrmNumArgsErr ann IsEq args

evalLambda :: Ann -> Args -> Eval ExprAnn
evalLambda ann (L.Cons params (L.Cons body L.Nil)) = do
  case params of
    ExprAnn (Lst params') _ -> do
      env <- getEnv
      pure $ ExprAnn (Fn env params' body) ann
    _ ->
      throwWrongTipeErr ann LstTipe (toExprTipe params)
evalLambda ann args = throwSFrmNumArgsErr ann Lambda args

evalQuote :: Ann -> Args -> Eval ExprAnn
evalQuote ann (L.Cons expr L.Nil) = pure $ expr
evalQuote ann args = throwSFrmNumArgsErr ann Quote args

evalRest :: Ann -> Args -> Eval ExprAnn
evalRest ann (L.Cons e L.Nil) = do
  xs'@(ExprAnn xs _) <- eval e
  case xs of
    Lst L.Nil ->
      pure $ ExprAnn (Lst L.Nil) ann
    Lst (L.Cons _ t) ->
      pure $ ExprAnn (Lst t) ann
    _ -> throwWrongTipeErr ann LstTipe (toExprTipe xs')
evalRest ann args = throwSFrmNumArgsErr ann Rest args

evalLst :: Ann -> Args -> Eval ExprAnn
evalLst ann (L.Cons x xs) = do
  expr@(ExprAnn fn _) <- eval x
  args <- traverse eval xs
  case fn of
    Fn localEnv params body -> do
      applyLambda ann params args body
    _ ->
      throwWrongTipeErr ann FnTipe (toExprTipe expr)
evalLst ann L.Nil = throwEmptyFnApplicationErr ann

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
    toParamName expr@(ExprAnn param _) =
      case param of
        Sym name ->
          pure name
        _ ->
          throwWrongTipeErr ann SymTipe (toExprTipe expr)
