module Eval 
  ( ErrName(..)
  , Eval
  , EvalErr(..)
  , EvalT
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
import Data.List (List(..), head, zipWith)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (class Traversable, sequence, traverse)
import Data.Tuple (Tuple(..), fst)
import Data.Unit (unit)
import ExprAnn (Ann, Env, Expr(..), ExprAnn(..), SrcSpan)

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
  show _ = "EvalErr"

throw :: forall m e a. Monad m => ErrName -> Ann -> EvalT m e a
throw name ann = throwError $ EvalErr name ann.srcSpan

newtype EvalState env = EvalState { env :: env }
derive instance evalStateNewtype :: Newtype (EvalState a) _

getEnv :: forall m e a. Monad m => EvalT m Env Env
getEnv = S.get >>= (unwrap >>> _.env >>> pure)

runMany :: Env -> List ExprAnn -> Identity (Tuple (Either EvalErr (List ExprAnn)) Env)
runMany env exprs = run env $ evalMany exprs

runOne :: Env -> ExprAnn -> Identity (Tuple (Either EvalErr ExprAnn) Env)
runOne env expr = run env $ evalOne expr

run :: forall a. Env -> Eval a -> Identity (Tuple (Either EvalErr a) Env)
run env evaled = resultWithEnv <$> rn evaled evalState
  where
    evalState = EvalState { env: env }
    rn = runStateT <<< runExceptT <<< unwrap

resultWithEnv :: forall a b. Tuple a (EvalState Env) -> Tuple a Env
resultWithEnv (Tuple res (EvalState { env })) = Tuple res env

evalMany :: List ExprAnn -> Eval (List ExprAnn)
evalMany = sequence <<< map eval

evalOne :: ExprAnn -> Eval ExprAnn
evalOne = eval

eval :: ExprAnn -> Eval ExprAnn
eval (ExprAnn expr ann) = throw NotImplemented ann

{-- evalSym :: Ann -> String -> Eval ExprAnn --}
{-- evalSym ann name = do --}
{--   env <- getEnv --}
{--   case M.lookup name env of --}
{--     Just expr -> --}
{--       pure expr --}
{--     Nothing -> --}
{--       throw (UnknownVar name) ann --}

{-- evalSFrm :: E.SFrm -> E.Ann -> Args -> Eval E.Expr --}
{-- evalSFrm _ ann Nil = throw NumArgs ann --}
{-- evalSFrm E.Quote ann args = evalQuote ann args --}
{-- evalSFrm E.First ann args = evalFirst ann args --}
{-- evalSFrm E.Rest ann args = evalRest ann args --}
{-- evalSFrm E.If ann args = evalIf ann args --}
{-- evalSFrm E.Def ann args = evalDef ann args --}
{-- evalSFrm E.Cons ann args = evalCons ann args --}
{-- evalSFrm E.IsAtm ann args = evalIsAtm ann args --}
{-- evalSFrm E.IsEq ann args = evalIsEq ann args --}
{-- evalSFrm E.Lambda ann args = evalLambda ann args --}

{-- evalFirst :: forall a. E.Ann -> Args -> Eval E.Expr --}
{-- evalFirst ann (Cons (Attr attr) Nil) = do --}
{--   arg <- attr.attribute --}
{--   case E.unExpr arg of --}
{--     E.Lst Nil -> throw LstLength ann --}
{--     E.Lst (Cons expr _) -> pure expr --}
{--     _ -> throw WrongTipe ann --}
{-- evalFirst ann _ = throw NumArgs ann --}

{-- evalRest :: E.Ann -> Args -> Eval E.Expr --}
{-- evalRest ann (Cons (Attr attr) Nil) = do --}
{--   arg <- attr.attribute --}
{--   case E.unExpr arg of --}
{--     E.Lst Nil -> --}
{--       pure arg --}
{--     E.Lst (Cons _ t) -> --}
{--       pure $ E.mkExpr (E.Lst t) ann --}
{--     _ -> throw WrongTipe ann --}
{-- evalRest ann _ = throw NumArgs ann --}

{-- evalIf :: E.Ann -> Args -> Eval E.Expr --}
{-- evalIf ann (Cons (Attr p) (Cons (Attr e1) (Cons (Attr e2) _))) = do --}
{--   p' <- p.attribute --}
{--   case E.unExpr p' of --}
{--     E.Lst Nil -> e2.attribute --}
{--     _ -> e1.attribute --}
{-- evalIf ann _ = throw NumArgs ann --}

{-- evalDef :: E.Ann -> Args -> Eval E.Expr --}
{-- evalDef ann (Cons (Attr sym) (Cons (Attr expr) Nil)) = do --}
{--   sym' <- sym.attribute --}
{--   case E.unExpr sym' of --}
{--     E.Sym name -> do --}
{--       val <- expr.attribute --}
{--       _ <- updateEnv name val --}
{--       pure val --}
{--     _ -> --}
{--       throw WrongTipe ann --}
{-- evalDef ann _ = throw NumArgs ann --}

{-- evalCons :: E.Ann -> Args -> Eval E.Expr --}
{-- evalCons ann (Cons (Attr h) (Cons (Attr t) Nil)) = do --}
{--   h' <- h.attribute --}
{--   t' <- t.attribute --}
{--   case E.unExpr t' of --}
{--     (E.Lst t'') -> --}
{--       pure $ E.mkExpr (E.Lst (Cons h' t'')) ann --}
{--     _ -> --}
{--       throw WrongTipe ann --}
{-- evalCons ann _ = throw NumArgs ann --}

{-- evalIsAtm :: E.Ann -> Args -> Eval E.Expr --}
{-- evalIsAtm ann (Cons (Attr h) Nil) = do --}
{--   h' <- h.attribute --}
{--   case E.unExpr h' of --}
{--     E.Sym _ -> --}
{--       pure $ E.mkExpr (E.Sym "t") ann --}
{--     E.Lst Nil -> --}
{--       pure $ E.mkExpr (E.Sym "t") ann --}
{--     _ -> --}
{--       pure $ E.mkExpr (E.Lst Nil) ann --}
{-- evalIsAtm ann _ = throw NumArgs ann --}

{-- evalIsEq :: E.Ann -> Args -> Eval E.Expr --}
{-- evalIsEq ann (Cons (Attr e1) (Cons (Attr e2) Nil)) = do --}
{--   e1' <- e1.attribute --}
{--   e2' <- e2.attribute --}
{--   case Tuple (E.unExpr e1') (E.unExpr e2') of --}
{--     Tuple (E.Sym name1) (E.Sym name2) -> --}
{--       if name1 == name2 --}
{--       then pure $ E.mkExpr (E.Sym "t") ann --}
{--       else pure $ E.mkExpr (E.Lst Nil) ann --}
{--     Tuple (E.Lst Nil) (E.Lst Nil) -> --}
{--       pure $ E.mkExpr (E.Sym "t") ann --}
{--     _ -> --}
{--       pure $ E.mkExpr (E.Lst Nil) ann --}
{-- evalIsEq ann _ = throw NumArgs ann --}

{-- evalLambda :: E.Ann -> Args -> Eval E.Expr --}
{-- evalLambda ann (Cons params (Cons body Nil)) = do --}
{--   let params' = unfoldAttr params --}
{--   let body' = unfoldAttr body --}
{--   case E.unExpr params' of --}
{--     E.Lst p -> do --}
{--       env <- getEnv --}
{--       pure $ E.mkExpr (E.Fn env p body') ann --}
{--     _ -> --}
{--       throw WrongTipe ann --}
{-- evalLambda ann _ = throw NumArgs ann --}

{-- evalQuote :: forall a. E.Ann -> Args -> Eval E.Expr --}
{-- evalQuote ann (Cons attr Nil) = pure $ unfoldAttr attr --}
{-- evalQuote ann _ = throw NumArgs ann --}

{-- updateEnv :: String -> E.Expr -> Eval Unit --}
{-- updateEnv key val = do --}
{--   EvalState evalState <- S.get --}
{--   S.put $ EvalState (evalState { env = M.insert key val evalState.env }) --}

{-- evalLst :: E.Ann -> Args -> Eval E.Expr --}
{-- evalLst ann (Cons (Attr x) xs) = do --}
{--   fn <- x.attribute --}
{--   args <- traverse (_.attribute <<< unwrap) xs --}
{--   case E.unExpr fn of --}
{--     E.Fn localEnv params body -> do --}
{--       expr <- applyLambda ann params args body --}
{--       let Attr attr = eval expr --}
{--       attr.attribute --}
{--     _ -> --}
{--       throw NotFn ann --}
{-- evalLst ann Nil = throw NumArgs ann --}

{-- applyLambda :: E.Ann -> List E.Expr -> List E.Expr -> E.Expr -> Eval E.Expr --}
{-- applyLambda ann params args body = do --}
{--   env <- bindArgs ann params args --}
{--   let evalState = EvalState { env: env } --}
{--   EvalT $ mapExceptT (withStateT $ const evalState) (pure body) --}

{-- bindArgs :: E.Ann -> List E.Expr -> List E.Expr -> Eval (E.Env E.Expr) --}
{-- bindArgs ann params args = do --}
{--   env <- getEnv --}
{--   bindings <- sequence (zipWith toBinding params args) --}
{--   pure $ M.fromFoldable bindings <> env --}
{--   where --}
{--     toBinding :: E.Expr -> E.Expr -> Eval (Tuple String E.Expr) --}
{--     toBinding param arg = do --}
{--       paramName <- toParamName param --}
{--       pure $ Tuple paramName arg --}

{--     toParamName :: E.Expr -> Eval String --}
{--     toParamName param = --}
{--       case E.unExpr' param of --}
{--         E.ExprAnnF (E.Sym name) _ -> --}
{--           pure name --}
{--         E.ExprAnnF _ ann -> --}
{--           throw Unknown ann --}
