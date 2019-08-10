module Test.Parser (spec) where

import Prelude

import Control.Lazy (fix)
import Data.Array ((:))
import Data.Char.Gen (genAlpha, genDigitChar, genUnicodeChar)
import Data.Either (Either(..))
import Data.Foldable (elem)
import Data.NonEmpty ((:|), singleton)
import Data.String (length)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Effect.Aff (Aff)
import ExprAnn (Ann, Expr(..), ExprAnn(..), SFrm(..), SrcLoc )
import Parser (parseOne)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, fail)
import Test.Spec.QuickCheck (quickCheck)
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen (Gen, arrayOf, elements, oneOf, suchThat, resize)

spec :: Spec Unit
spec = do
  describe "Parser" do
    describe "Parser.parseOne" do
      describe "symbol" do
        it "parses a symbol" $ do
          quickCheck prop_parseOne_Sym
      describe "list" do
        it "parses a list" $ do
          quickCheck prop_parseOne_Lst
      describe "special forms" do
        it "parses atom?" do
          "atom?" `parsesOneSFrmTo` (SFrm IsAtm)
        it "parses first" do
          "first" `parsesOneSFrmTo` (SFrm First)
        it "parses rest" do
          "rest" `parsesOneSFrmTo` (SFrm Rest)
        it "parses ::" do
          "::" `parsesOneSFrmTo` (SFrm Cons)
        it "parses =" do
          "=" `parsesOneSFrmTo` (SFrm Def)
        it "parses ==" do
          "==" `parsesOneSFrmTo` (SFrm IsEq)
        it "parses fn" do
          "fn" `parsesOneSFrmTo` (SFrm Lambda)
        it "parses quote" do
          "quote" `parsesOneSFrmTo` (SFrm Quote)
        it "parses if" do
          "if" `parsesOneSFrmTo` (SFrm If)

exprAnn :: Expr -> SrcLoc -> SrcLoc -> ExprAnn
exprAnn expr begin end = ExprAnn expr $ ann begin end

ann :: SrcLoc -> SrcLoc -> Ann
ann begin end = { srcSpan: { begin: begin, end: end } }

srcLoc :: Int -> Int -> SrcLoc
srcLoc line column = { line: line, column: column }

parsesOneSFrmTo :: String -> Expr -> Aff Unit
parsesOneSFrmTo = parsesSFrm parseOneSucceedsWith

parsesSFrm :: (String -> ExprAnn -> Aff Unit) -> String -> Expr -> Aff Unit
parsesSFrm succeedsWith str expr =
  str `succeedsWith` exprAnn expr begin end
  where
    begin = srcLoc 1 1
    end = srcLoc 1 $ (length str + 1)

parseOneSucceedsWith :: String -> ExprAnn -> Aff Unit
parseOneSucceedsWith str expected =
  case parseOne str of
    Left err -> fail $ show err
    Right actual -> actual `shouldEqual` expected

newtype ArbLst = ArbLst String
derive instance eqArbLst :: Eq ArbLst

instance arbitraryArbLst :: Arbitrary ArbLst where
  arbitrary = ArbLst <$> genLst

prop_parseOne_Lst :: ArbLst -> Boolean
prop_parseOne_Lst (ArbLst lst) =
  case parseOne lst of
    Right (ExprAnn (Lst _) _) -> true
    _ -> false

genSexpr :: Gen String
genSexpr = resize 2 $ (fix $ \p -> oneOf $ genAtom :| [inParens p])

genLst :: Gen String
genLst = inParens genSexpr

genAtom :: Gen String
genAtom = oneOf $ singleton genSym

genLineComment :: Gen String
genLineComment = do
  content <- fromCharArray <$> arrayOf genUnicodeChar
  pure $ ";" <> content <> "\n"

tokenOf :: Gen String -> Gen String
tokenOf gen = do
  s1 <- genSpace
  x <- gen
  s2 <- genSpace
  pure $ s1 <> x <> s2
  where genSpace = oneOf $ genLineComment :| [genWhitespace]
        genWhitespace = fromCharArray <$> arrayOf genWhitespaceChar
        genWhitespaceChar = elements $ ' ' :| toCharArray "\t\n\r"

inParens :: Gen String -> Gen String
inParens gen = do
  p1 <- tokenOf $ pure "("
  x <- gen
  p2 <- tokenOf $ pure ")"
  pure $ p1 <> x <> p2

prop_parseOne_Sym :: ArbSym -> Boolean
prop_parseOne_Sym (ArbSym sym) =
  case parseOne sym of
    Right (ExprAnn expr _) -> expr == Sym sym
    _ -> false

newtype ArbSym = ArbSym String
derive instance eqArbSym :: Eq ArbSym

instance arbitraryArbSym :: Arbitrary ArbSym where
  arbitrary = ArbSym <$> genSym

genSym :: Gen String
genSym =
  flip suchThat isNotSFrm $ do
     f <- genFirstChar
     r <- arrayOf genRestChar
     pure $ fromCharArray (f:r)
  where
    genFirstChar :: Gen Char
    genFirstChar = oneOf $ genAlpha :| [genFirstSpecialChar]

    genFirstSpecialChar :: Gen Char
    genFirstSpecialChar = elements $ '!' :| toCharArray "$%&*/:<=>?~_^"

    genRestChar :: Gen Char
    genRestChar = oneOf $ genFirstChar :| [genDigitChar, genRestSpecialChar]

    genRestSpecialChar :: Gen Char
    genRestSpecialChar = elements $ '.' :| toCharArray "+-"

isNotSFrm :: String -> Boolean
isNotSFrm x = not $ elem x sFrms
  where sFrms = ["::", "=", "==", "atom?", "first", "fn", "if", "quote", "rest"]
