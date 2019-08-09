module Test.Parser (spec) where

import Prelude

import Data.Either (Either(..))
import Data.List (List)
import Effect.Aff (Aff)
import ExprAnn (Ann, Expr(..), ExprAnn(..), SFrm(..), SrcLoc )
import Parser (parseStr)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, fail)

spec :: Spec Unit
spec = do
  describe "Parser" do
    describe "Parser.parseStr" do
      describe "special forms" do
        it "parses atom?" do
          let begin = srcLoc 1 1
          let end = srcLoc 1 6
          let expr = SFrm IsAtm
          "atom?" `parsesExprTo` exprAnn expr begin end
        it "parses first" do
          let begin = srcLoc 1 1
          let end = srcLoc 1 6
          let expr = SFrm First
          "first" `parsesExprTo` exprAnn expr begin end
        it "parses rest" do
          let begin = srcLoc 1 1
          let end = srcLoc 1 5
          let expr = SFrm Rest
          "rest" `parsesExprTo` exprAnn expr begin end
        it "parses ::" do
          let begin = srcLoc 1 1
          let end = srcLoc 1 3
          let expr = SFrm Cons
          "::" `parsesStrTo` (pure $ exprAnn expr begin end)
        it "parses =" do
          let begin = srcLoc 1 1
          let end = srcLoc 1 2
          let expr = SFrm Def
          "=" `parsesExprTo` exprAnn expr begin end
        it "parses ==" do
          let begin = srcLoc 1 1
          let end = srcLoc 1 3
          let expr = SFrm IsEq
          "==" `parsesExprTo` exprAnn expr begin end
        it "parses fn" do
          let begin = srcLoc 1 1
          let end = srcLoc 1 3
          let expr = SFrm Lambda
          "fn" `parsesExprTo` exprAnn expr begin end
        it "parses quote" do
          let begin = srcLoc 1 1
          let end = srcLoc 1 6
          let expr = SFrm Quote
          "quote" `parsesExprTo` exprAnn expr begin end
        it "parses if" do
          let begin = srcLoc 1 1
          let end = srcLoc 1 3
          let expr = SFrm If
          "if" `parsesExprTo` exprAnn expr begin end

exprAnn :: Expr -> SrcLoc -> SrcLoc -> ExprAnn
exprAnn expr begin end = ExprAnn expr $ ann begin end

ann :: SrcLoc -> SrcLoc -> Ann
ann begin end = { srcSpan: { begin: begin, end: end } }

srcLoc :: Int -> Int -> SrcLoc
srcLoc line column = { line: line, column: column }

parsesExprTo :: String -> ExprAnn -> Aff Unit
parsesExprTo str expected = parsesStrTo str $ pure expected

parsesStrTo :: String -> List ExprAnn -> Aff Unit
parsesStrTo str expected =
  case parseStr str of
    Left err -> fail $ show err
    Right actual -> actual `shouldEqual` expected
