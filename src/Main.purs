module Main where

import Prelude (Unit, map, mempty, pure, show, unit, (>>>))

import Data.Array as A
import Data.Either (Either)
import Effect (Effect)
import Run as Run

main :: Effect Unit
main = pure unit

runFromJs :: Array String -> Array (Either Run.RunErr String)
runFromJs = run' >>> showResults
  where run' = Run.runInSharedEnv Run.runOne mempty
        showResults = _.results >>> map (map show) >>> A.reverse
