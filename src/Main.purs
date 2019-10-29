module Main where

import Prelude

import Debug.Trace (spy)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Coroutine (Yield(..), bounce)
import Lang.Core
import Lang.Eval (eval)
import Lang.Parser (parseOne)

src = "(do (pause) (define id (lambda (x) x)) (pause) (define foo 123) (do (cons 'x '()) (pause) (car '(z y x))) (pause) (cons '(x) '(y)))"

bindings :: forall m. Bindings (PrimFns m)
bindings = mempty

main :: Effect Unit
main = do
  Tuple final _ <- runEvalT bindings do
    case parseOne src of
      Left parseErr -> do
        let _ = spy "parseErr" parseErr
        pure unit
      Right ast -> do
        let _ = spy "parsed" ast
        x <- bounce $ eval ast
        case x of
          Left (Yield env continuation) -> do
            let _ = spy "env1" env
            y <- bounce $ continuation unit
            case y of
              Left (Yield env continuation) -> do
                let _ = spy "env2" env
                z <- bounce $ continuation unit
                case z of
                  Left (Yield env continuation) -> do
                    let _ = spy "env3" env
                    a <- bounce $ continuation unit
                    case a of
                      Left (Yield env continuation) -> do
                        let _ = spy "env4" env
                        pure unit
                      Right r -> do
                        let _ = spy "done4" (show r)
                        pure unit
                  Right r -> do
                    let _ = spy "done3" (show r)
                    pure unit
              Right r -> do
                let _ = spy "done2" (show r)
                pure unit
          Right r -> do
            let _ = spy "done1" (show r)
            pure unit
  let _ = spy "final" (show final)
  pure unit
