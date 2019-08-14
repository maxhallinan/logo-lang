module Main where

import Prelude (class Show, map, mempty, show, ($), (>>>))

import Control.Comonad (class Comonad, extract)
import Data.Array as Arr
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Foldable as F
import Data.List (List)
import Eval as Eval
import ExprAnn as ExprAnn
import Text.Parsing.Parser as P
import Parser as Parser

data RunErr
  = ParseErr P.ParseError
  | EvalErr Eval.EvalErr

instance showRunErr :: Show RunErr where
  show (ParseErr parseErr) = show parseErr
  show (EvalErr evalErr) = show evalErr

type ResultWithEnv a = { env :: ExprAnn.Env, result :: Either RunErr a }

runOne :: ExprAnn.Env -> String -> ResultWithEnv ExprAnn.ExprAnn
runOne = run Parser.parseOne Eval.runOne

runMany :: ExprAnn.Env -> String -> ResultWithEnv (List ExprAnn.ExprAnn)
runMany = run Parser.parseMany Eval.runMany

run
  :: forall m a
   . Comonad m
  => (String -> Either P.ParseError a)
  -> (ExprAnn.Env -> a -> m (Eval.ResultWithEnv a))
  -> ExprAnn.Env
  -> String
  -> ResultWithEnv a
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

runInSharedEnv
  :: forall a
   . (ExprAnn.Env -> String -> ResultWithEnv a)
  -> ExprAnn.Env
  -> Array String
  -> { env :: ExprAnn.Env, results :: Array (Either RunErr a) }
runInSharedEnv run' env = F.foldl worker { env: env, results: mempty }
  where
    worker
      :: { env :: ExprAnn.Env, results :: Array (Either RunErr a) }
      -> String
      -> { env :: ExprAnn.Env, results :: Array (Either RunErr a) }
    worker accum program =
      { env: ran.env
      , results: Arr.cons ran.result accum.results
      }
      where ran = run' accum.env program

runFromJs :: Array String -> Array (Either RunErr String)
runFromJs = run' >>> showResults
  where run' = runInSharedEnv runOne mempty
        showResults = _.results >>> map (map show) >>> Arr.reverse
