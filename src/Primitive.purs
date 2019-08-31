module Primitive (builtIns) where

import Prelude

import Data.Map as M
import Data.Tuple (Tuple(..))
import ExprAnn (Src(..), Env, Op1(..), Op2(..), boolOp1, boolOp2, mkTrue, primOp1, primOp2)

builtIns :: Env
builtIns = M.fromFoldable
  [ Tuple "and" (primOp2 OpAnd $ boolOp2 (&&))
  , Tuple "else" (mkTrue { src: Primitive })
  , Tuple "not" (primOp1 OpNot $ boolOp1 not)
  , Tuple "or" (primOp2 OpOr $ boolOp2 (||))
  ]
