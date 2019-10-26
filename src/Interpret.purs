module Interpret (InterpretErr, interpretMany) where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.List (List)
import Core as Core
import Eval as Eval
import Parser as Parser

data InterpretErr
  = ParseErr Parser.ParseErr
  | EvalErr Core.EvalErr

instance showInterpretErr :: Show InterpretErr where
  show (ParseErr parseErr) = show parseErr
  show (EvalErr evalErr) = show evalErr

interpretMany
  :: forall m
   . Monad m
  => Core.Env
  -> String
  -> m (Either InterpretErr (List Core.ExprAnn))
interpretMany bnds src =
  case Parser.parseMany src of
    Left e ->
      pure $ Left $ ParseErr e
    Right x -> do
      { result } <- Eval.runMany bnds x
      pure $ lmap EvalErr result
