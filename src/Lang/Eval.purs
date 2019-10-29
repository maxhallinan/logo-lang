module Lang.Eval (eval) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except.Trans (mapExceptT)
import Control.Monad.State.Trans (withStateT)
import Control.Monad.State.Trans as S
import Control.Monad.Trans.Class (lift)
import Coroutine (CoroutineT(..), done, yield)
import Data.Foldable (all, length)
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import Lang.Core
  ( Ann
  , Bindings(..)
  , Env
  , ErrTipe(..)
  , Expr(..)
  , ExprAnn(..)
  , ExprTipe(..)
  , Eval
  , EvalErr
  , EvalState(..)
  , EvalT(..)
  , PrimFns
  , SFrm(..)
  , isTrue
  , getEnv
  , mkFalse
  , mkTrue
  , sfrmNumArgs
  , throw
  , toExprTipe
  , updateEnv
  )

numArgsErr :: Int -> Int -> ErrTipe
numArgsErr expected received = NumArgs { expected, received }

throwNumArgsErr
  :: forall m a
   . MonadThrow EvalErr m
  => Ann
  -> Int
  -> Int
  -> m a
throwNumArgsErr ann expected received =
  throw ann $ numArgsErr expected received

throwSFrmNumArgsErr
  :: forall m a
   . MonadThrow EvalErr m
  => Ann
  -> SFrm
  -> Args
  -> m a
throwSFrmNumArgsErr ann sfrm args = throwNumArgsErr ann expected received
  where expected = sfrmNumArgs sfrm
        received = length args

wrongTipeErr :: ExprTipe -> ExprTipe -> ErrTipe
wrongTipeErr expected received = WrongTipe { expected, received }

throwWrongTipeErr
  :: forall m a
   . MonadThrow EvalErr m
  => Ann
  -> ExprTipe
  -> ExprTipe
  -> m a
throwWrongTipeErr ann expected received =
  throw ann $ wrongTipeErr expected received

lstLengthErr :: Int -> Int -> ErrTipe
lstLengthErr expected received = LstLength { expected, received }

throwLstLengthErr
  :: forall m a
   . MonadThrow EvalErr m
  => Ann
  -> Int
  -> Int
  -> m a
throwLstLengthErr ann expected received =
  throw ann $ lstLengthErr expected received

unknownVarErr :: String -> ErrTipe
unknownVarErr varName = UnknownVar { varName }

throwUnknownVarErr
  :: forall m a
   . MonadThrow EvalErr m
  => Ann
  -> String
  -> m a
throwUnknownVarErr ann varName =
  throw ann $ unknownVarErr varName

unknownErr :: ErrTipe
unknownErr = UnknownErr

throwUnknownErr
  :: forall m a
   . MonadThrow EvalErr m
  => Ann
  -> m a
throwUnknownErr ann = throw ann unknownErr

emptyFnApplicationErr :: ErrTipe
emptyFnApplicationErr = EmptyFnApplication

throwEmptyFnApplicationErr
  :: forall m a
   . MonadThrow EvalErr m
  => Ann
  -> m a
throwEmptyFnApplicationErr ann = throw ann emptyFnApplicationErr

trueCondClauseNotFound :: ErrTipe
trueCondClauseNotFound = TrueCondClauseNotFound

throwTrueCondClauseNotFound
  :: forall m a
   . MonadThrow EvalErr m
  => Ann
  -> m a
throwTrueCondClauseNotFound ann = throw ann trueCondClauseNotFound

eval :: forall m. Monad m => ExprAnn -> Eval m ExprAnn
eval exprAnn@(ExprAnn expr ann) =
  case expr of
    (Sym name) ->
      evalSym ann name
    (Lst (L.Cons (ExprAnn (SFrm sfrm) _) args)) ->
      evalSFrm ann sfrm args
    (Lst xs) ->
      evalLst ann xs
    (Float _) -> do
      done exprAnn
    (Integer _) ->
      done exprAnn
    _ ->
      lift $ throwUnknownErr ann

evalSym :: forall m. Monad m => Ann -> String -> Eval m ExprAnn
evalSym ann name = do
  env <- lift $ getEnv
  case M.lookup name env of
    Just expr -> do
      done expr
    Nothing ->
      lift $ throwUnknownVarErr ann name

type Args = L.List ExprAnn

evalSFrm :: forall m. Monad m => Ann -> SFrm -> Args -> Eval m ExprAnn
evalSFrm ann Conz args = evalConz ann args
evalSFrm ann Car args = evalCar ann args
evalSFrm ann Cdr args = evalCdr ann args
evalSFrm ann Cond args = evalCond ann args
evalSFrm ann Def args = evalDef ann args
evalSFrm ann Do args = evalDo ann args
evalSFrm ann If args = evalIf ann args
evalSFrm ann IsAtm args = evalIsAtm ann args
evalSFrm ann IsEq args = evalIsEq ann args
evalSFrm ann Lambda args = evalLambda ann args
evalSFrm ann Pause args = evalPause ann args
evalSFrm ann Quote args = evalQuote ann args
evalSFrm ann sfrm L.Nil = lift $ throwSFrmNumArgsErr ann sfrm L.Nil

evalConz :: forall m. Monad m => Ann -> Args -> Eval m ExprAnn
evalConz ann (L.Cons e1 (L.Cons e2 L.Nil)) = do
  h <- eval e1
  t <- eval e2
  case t of
    (ExprAnn (Lst t') _) ->
      done $ ExprAnn (Lst (L.Cons h t')) ann
    _ ->
      lift $ throwWrongTipeErr ann LstTipe (toExprTipe t)
evalConz ann args = lift $ throwSFrmNumArgsErr ann Conz args

evalDef :: forall m. Monad m => Ann -> Args -> Eval m ExprAnn
evalDef ann (L.Cons e1 (L.Cons e2 L.Nil)) = do
  val <- eval e2
  case e1 of
    ExprAnn (Sym name) _ -> do
      _ <- lift $ updateEnv name val
      done val
    _ ->
      lift $ throwWrongTipeErr ann SymTipe (toExprTipe e1)
evalDef ann args = lift $ throwSFrmNumArgsErr ann Def args

evalCar :: forall m. Monad m => Ann -> Args -> Eval m ExprAnn
evalCar ann (L.Cons e L.Nil) = do
  expr@(ExprAnn xs _) <- eval e
  case xs of
    Lst L.Nil ->
      lift $ throwLstLengthErr ann 1 0
    Lst (L.Cons h _) ->
      done h
    _ ->
      lift $ throwWrongTipeErr ann LstTipe (toExprTipe expr)
evalCar ann args = lift $ throwSFrmNumArgsErr ann Car args

evalDo :: forall m. Monad m => Ann -> Args -> Eval m ExprAnn
evalDo ann args = do
  results <- traverse eval args
  case L.last results of
    Nothing ->
      lift $ throwSFrmNumArgsErr ann Do L.Nil
    Just e ->
      done e

evalIf :: forall m. Monad m => Ann -> Args -> Eval m ExprAnn
evalIf ann (L.Cons p (L.Cons e1 (L.Cons e2 L.Nil))) = do
  p' <- eval p
  if isTrue p'
  then eval e1
  else eval e2
evalIf ann args = lift $ throwSFrmNumArgsErr ann If args

evalIsAtm :: forall m. Monad m => Ann -> Args -> Eval m ExprAnn
evalIsAtm ann (L.Cons e L.Nil) = do
  ExprAnn x _ <- eval e
  case x of
    Float _ ->
      done t
    Integer _ ->
      done t
    Sym _ ->
      done t
    _ ->
      done f
  where
    t = mkTrue ann
    f = mkFalse ann
evalIsAtm ann args = lift $ throwSFrmNumArgsErr ann IsAtm args

evalIsEq :: forall m. Monad m => Ann -> Args -> Eval m ExprAnn
evalIsEq ann (L.Cons e1 (L.Cons e2 L.Nil)) = do
  ExprAnn x _ <- eval e1
  ExprAnn y _ <- eval e2
  evalIsEqExpr x y
  where
    t = mkTrue ann
    f = mkFalse ann
    evalIsEqExpr :: Expr -> Expr -> Eval m ExprAnn
    evalIsEqExpr x y = do
      case Tuple x y of
        Tuple (Float n1) (Float n2) ->
          if n1 == n2
          then done t
          else done f
        Tuple (Integer n1) (Integer n2) ->
          if n1 == n2
          then done t
          else done f
        Tuple (Sym name1) (Sym name2) ->
          if name1 == name2
          then done t
          else done f
        Tuple (Lst L.Nil) (Lst L.Nil) ->
          done t
        Tuple (Lst xs) (Lst ys) ->
          evalIsEqLst xs ys
        _ ->
          done f
    evalIsEqLst :: L.List ExprAnn -> L.List ExprAnn -> Eval m ExprAnn
    evalIsEqLst xs ys = do
      equalities <- traverse evalIsEqExprPair (L.zip xs ys)
      let isNotEmpty = not $ L.null equalities
      let isAllEqual = all isTrue equalities
      if isNotEmpty && isAllEqual
      then done t
      else done f
    evalIsEqExprPair :: Tuple ExprAnn ExprAnn -> Eval m ExprAnn
    evalIsEqExprPair (Tuple (ExprAnn x _) (ExprAnn y _)) = evalIsEqExpr x y
evalIsEq ann args = lift $ throwSFrmNumArgsErr ann IsEq args

evalLambda :: forall m. Monad m => Ann -> Args -> Eval m ExprAnn
evalLambda ann (L.Cons params (L.Cons body L.Nil)) = do
  case params of
    ExprAnn (Lst params') _ -> do
      env <- lift $ getEnv
      done $ ExprAnn (Fn env params' body) ann
    _ ->
      lift $ throwWrongTipeErr ann LstTipe (toExprTipe params)
evalLambda ann args = lift $ throwSFrmNumArgsErr ann Lambda args

evalQuote :: forall m. Monad m => Ann -> Args -> Eval m ExprAnn
evalQuote ann (L.Cons expr L.Nil) = done expr
evalQuote ann args = lift $ throwSFrmNumArgsErr ann Quote args

evalCdr :: forall m. Monad m => Ann -> Args -> Eval m ExprAnn
evalCdr ann (L.Cons e L.Nil) = do
  xs'@(ExprAnn xs _) <- eval e
  case xs of
    Lst L.Nil ->
      done $ ExprAnn (Lst L.Nil) ann
    Lst (L.Cons _ t) ->
      done $ ExprAnn (Lst t) ann
    _ -> lift $ throwWrongTipeErr ann LstTipe (toExprTipe xs')
evalCdr ann args = lift $ throwSFrmNumArgsErr ann Cdr args

evalCond :: forall m. Monad m => Ann -> Args -> Eval m ExprAnn
evalCond ann L.Nil = lift $ throwSFrmNumArgsErr ann Cond L.Nil
evalCond ann clauses = go clauses
  where
    go :: Args -> Eval m ExprAnn
    go (L.Cons (ExprAnn (Lst (L.Cons predicate (L.Cons consequent L.Nil))) _) rest) = do
      p <- isTrue <$> eval predicate
      if p
      then eval consequent
      else go rest
    go (L.Cons (ExprAnn (Lst xs) clauseAnn) _) = lift $ throwLstLengthErr clauseAnn 2 (length xs)
    go (L.Cons expr _) = lift $ throwWrongTipeErr ann LstTipe (toExprTipe expr)
    go L.Nil = lift $ throwTrueCondClauseNotFound ann

evalPause :: forall m. Monad m => Ann -> Args -> Eval m ExprAnn
evalPause ann _ = do
  EvalState { bindings } <- lift S.get
  yield bindings (\_ -> done $ mkFalse ann)

evalLst :: forall m. Monad m => Ann -> Args -> Eval m ExprAnn
evalLst ann (L.Cons x xs) = do
  expr@(ExprAnn fn _) <- eval x
  args <- traverse eval xs
  case fn of
    Fn localEnv params body -> do
      applyLambda ann localEnv params args body
    _ ->
      lift $ throwWrongTipeErr ann FnTipe (toExprTipe expr)
evalLst ann L.Nil = lift $ throwEmptyFnApplicationErr ann

applyLambda :: forall m. Monad m => Ann -> Env -> L.List ExprAnn -> L.List ExprAnn -> ExprAnn -> Eval m ExprAnn
applyLambda ann localEnv params args body = do
  evalState <- bindArgs ann localEnv params args
  CoroutineT $ EvalT $ mapExceptT (withStateT $ const evalState) ((unwrap <<< unwrap) $ eval body)

bindArgs :: forall m. Monad m => Ann -> Env -> L.List ExprAnn -> L.List ExprAnn -> Eval m (EvalState (Bindings (PrimFns m)))
bindArgs ann localEnv params args = do
  fnEnv <- M.fromFoldable <$> sequence (L.zipWith toBinding params args)
  EvalState s@({ bindings: Bindings b }) <- lift $ S.get
  done $ EvalState $ s { bindings = Bindings $ b { env = fnEnv <> localEnv <> b.env } }
  where
    toBinding :: ExprAnn -> ExprAnn -> Eval m (Tuple String ExprAnn)
    toBinding param arg = do
      paramName <- toParamName param
      done $ Tuple paramName arg

    toParamName :: ExprAnn -> Eval m String
    toParamName expr@(ExprAnn param _) =
      case param of
        Sym name ->
          done name
        _ ->
          lift $ throwWrongTipeErr ann SymTipe (toExprTipe expr)
