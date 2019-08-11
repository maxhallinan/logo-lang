module Main where

import Prelude

import Control.Comonad (class Comonad, extract)
import Data.Bifunctor (lmap, bimap)
import Data.Either (Either(..))
import Data.List
import Data.Tuple as T
import Effect (Effect)
import Effect.Console (log)
import Eval as Eval
import ExprAnn as ExprAnn
import Text.Parsing.Parser as P
import Parser as Parser

data RunErr
  = ParseErr P.ParseError
  | EvalErr Eval.EvalErr

runOne :: String -> Either RunErr ExprAnn.ExprAnn
runOne = run Parser.parseOne (Eval.runOne mempty)

runMany :: String -> Either RunErr (List ExprAnn.ExprAnn)
runMany = run Parser.parseMany (Eval.runMany mempty)

run
  :: forall m a b
   . Comonad m
  => (String -> Either P.ParseError a)
  -> (a -> m (T.Tuple (Either Eval.EvalErr a) b))
  -> String
  -> Either RunErr a
run parse eval program = p program >>= e
  where
    p = lmap ParseErr <<< parse
    e = lmap EvalErr <<< T.fst <<< extract <<< eval
