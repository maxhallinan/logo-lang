module Main where

import Prelude (Unit, map, mempty, pure, show, unit, (>>>))

import Data.Array as A
import Data.Either (Either)
import Data.Foldable as F
import Effect (Effect)
import Eval as Eval
import ExprAnn as ExprAnn
import Run as Run

main :: Effect Unit
main = pure unit

type JsInput a = { id :: String, program :: a }

type JsOutput a = { id :: String, result :: Either Run.RunErr a }

runFromJs :: Array (JsInput String) -> Array (JsOutput String)
runFromJs = run' >>> showResults
  where run' = runInSharedEnv Run.runOne mempty
        showResults = _.results >>> map (\r -> { id: r.id, result: map show r.result }) >>> A.reverse

runInSharedEnv
  :: forall a
   . (ExprAnn.Env -> String -> Eval.ResultWithEnv Run.RunErr a)
  -> ExprAnn.Env
  -> Array (JsInput String)
  -> { env :: ExprAnn.Env, results :: Array (JsOutput a) }
runInSharedEnv run' env = F.foldl worker { env: env, results: mempty }
  where
    worker
      :: { env :: ExprAnn.Env, results :: Array (JsOutput a) }
      -> JsInput String
      -> { env :: ExprAnn.Env, results :: Array (JsOutput a) }
    worker accum i =
      { env: ran.env
      , results: A.cons ({ id: i.id, result: ran.result }) accum.results
      }
      where ran = run' accum.env i.program


showRunErr :: Run.RunErr -> String
showRunErr = show
