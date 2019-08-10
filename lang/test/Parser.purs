module Test.Parser (spec) where

import Prelude

import Control.Plus (empty)
import Data.Array ((:), fromFoldable)
import Data.Char.Unicode (toUpper)
import Data.Either (Either(..))
import Data.Foldable (elem, foldl)
import Data.NonEmpty ((:|), NonEmpty)
import Data.List (List)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Effect.Aff (Aff)
import ExprAnn (Ann, Expr(..), ExprAnn(..), SFrm(..), SrcLoc )
import Parser (parseOne)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, fail)
import Test.Spec.QuickCheck (quickCheck)
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen (Gen, arrayOf, elements, oneOf, suchThat)

spec :: Spec Unit
spec = do
  describe "Parser" do
    describe "Parser.parseOne" do
      describe "symbols" do
         it "parses symbols" $ do quickCheck prop_parseStr_Sym 
      describe "special forms" do
        it "parses atom?" do
          let begin = srcLoc 1 1
          let end = srcLoc 1 6
          let expr = SFrm IsAtm
          "atom?" `parsesOneTo` exprAnn expr begin end
        it "parses first" do
          let begin = srcLoc 1 1
          let end = srcLoc 1 6
          let expr = SFrm First
          "first" `parsesOneTo` exprAnn expr begin end
        it "parses rest" do
          let begin = srcLoc 1 1
          let end = srcLoc 1 5
          let expr = SFrm Rest
          "rest" `parsesOneTo` exprAnn expr begin end
        it "parses ::" do
          let begin = srcLoc 1 1
          let end = srcLoc 1 3
          let expr = SFrm Cons
          "::" `parsesOneTo` (exprAnn expr begin end)
        it "parses =" do
          let begin = srcLoc 1 1
          let end = srcLoc 1 2
          let expr = SFrm Def
          "=" `parsesOneTo` exprAnn expr begin end
        it "parses ==" do
          let begin = srcLoc 1 1
          let end = srcLoc 1 3
          let expr = SFrm IsEq
          "==" `parsesOneTo` exprAnn expr begin end
        it "parses fn" do
          let begin = srcLoc 1 1
          let end = srcLoc 1 3
          let expr = SFrm Lambda
          "fn" `parsesOneTo` exprAnn expr begin end
        it "parses quote" do
          let begin = srcLoc 1 1
          let end = srcLoc 1 6
          let expr = SFrm Quote
          "quote" `parsesOneTo` exprAnn expr begin end
        it "parses if" do
          let begin = srcLoc 1 1
          let end = srcLoc 1 3
          let expr = SFrm If
          "if" `parsesOneTo` exprAnn expr begin end

exprAnn :: Expr -> SrcLoc -> SrcLoc -> ExprAnn
exprAnn expr begin end = ExprAnn expr $ ann begin end

ann :: SrcLoc -> SrcLoc -> Ann
ann begin end = { srcSpan: { begin: begin, end: end } }

srcLoc :: Int -> Int -> SrcLoc
srcLoc line column = { line: line, column: column }

parsesOneTo :: String -> ExprAnn -> Aff Unit
parsesOneTo str expected =
  case parseOne str of
    Left err -> fail $ show err
    Right actual -> actual `shouldEqual` expected

prop_parseStr_Sym :: ArbSym -> Boolean
prop_parseStr_Sym (ArbSym sym) =
  case parseOne sym of
    Right (ExprAnn expr _) -> expr == Sym sym
    _ -> false

newtype ArbSym = ArbSym String
derive instance eqArbSym :: Eq ArbSym

instance arbitraryArbSym :: Arbitrary ArbSym where
  arbitrary = ArbSym <$> symbolGen

symbolGen :: Gen String
symbolGen = 
  flip suchThat isNotSFrm $ do
     i <- initial
     sub <- arrayOf subsequent
     pure $ fromCharArray (i : sub)
  where
    initial :: Gen Char
    initial = oneOf $ alpha :| [elements $ '!' :| toCharArray "$%&*/:<=>?~_^"]

    subsequent :: Gen Char
    subsequent = oneOf $ initial :| [numeric, elements $ '.' :| toCharArray "+-"]

isNotSFrm :: String -> Boolean
isNotSFrm x = not $ elem x sFrms
  where sFrms = ["::", "=", "==", "atom?", "first", "fn", "if", "quote", "rest"]

numbers :: NonEmpty Array Char
numbers =  '1' :| toCharArray "234567890"

numeric :: Gen Char
numeric = elements numbers

letters :: NonEmpty Array Char
letters = 'a' :| toCharArray "bcdefghijklmnopqrstuvwxyz"

alpha :: Gen Char
alpha = oneOf $ lowerAlpha :| [upperAlpha]

lowerAlpha :: Gen Char
lowerAlpha = elements letters

upperAlpha :: Gen Char
upperAlpha = elements $ map toUpper letters
