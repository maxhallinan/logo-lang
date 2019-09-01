module Run where

import Prelude (class Show, show, ($), (>>>))

import Control.Comonad (class Comonad, extract)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.List (List)
import Core as Core
import Eval as Eval
import Parser as Parser

type Result a = Eval.ResultWithEnv RunErr a

data RunErr
  = ParseErr Parser.ParseErr
  | EvalErr Eval.EvalErr

instance showRunErr :: Show RunErr where
  show (ParseErr parseErr) = show parseErr
  show (EvalErr evalErr) = show evalErr

runOne :: Core.Env -> String -> Eval.ResultWithEnv RunErr Core.ExprAnn
runOne = run Parser.parseOne Eval.evalOne

runMany :: Core.Env -> String -> Eval.ResultWithEnv RunErr (List Core.ExprAnn)
runMany = run Parser.parseMany Eval.evalMany

run
  :: forall m a
   . Comonad m
  => (String -> Either Parser.ParseErr a)
  -> (Core.Env -> a -> m (Eval.Result a))
  -> Core.Env
  -> String
  -> Eval.ResultWithEnv RunErr a
run parse eval initEnv = parse >>> either failWithParseErr evalWithEnv
  where
    failWithParseErr parseErr =
      { env: initEnv
      , result: Left $ ParseErr parseErr
      }
    evalWithEnv expr =
      { env: evaled.env
      , result: lmap EvalErr evaled.result
      }
      where evaled = extract $ eval initEnv expr

showRunError :: RunErr -> String
showRunError = show
