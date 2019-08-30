module Test.Parser (spec) where

import Prelude

import Control.Lazy (fix)
import Data.Array ((:))
import Data.Char.Gen (genAlpha, genDigitChar, genUnicodeChar)
import Data.Either (Either(..))
import Data.Foldable (elem)
import Data.List as L
import Data.NonEmpty ((:|), singleton)
import Data.String (length)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Effect.Aff (Aff)
import ExprAnn (Ann, Expr(..), ExprAnn(..), SFrm(..), SrcLoc )
import Parser (parseMany, parseOne)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, fail)
import Test.Spec.QuickCheck (quickCheck)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, arrayOf, elements, oneOf, suchThat, resize)

spec :: Spec Unit
spec = do
  describe "Parser" do
    describe "Parser.parseMany" do
      describe "integer" do
        it "parses a integer" $ do
          quickCheck prop_parseMany_Integer
      describe "float" do
        it "parses a float" $ do
          quickCheck prop_parseMany_Float
      describe "symbol" do
        it "parses a symbol" $ do
          quickCheck prop_parseMany_Sym
      describe "list" do
        it "parses a list" $ do
          quickCheck prop_parseMany_Lst
      describe "special forms" do
        it "parses atom?" do
          "atom?" `parsesManySFrmTo` (SFrm IsAtm)
        it "parses car" do
          "car" `parsesManySFrmTo` (SFrm Car)
        it "parses cdr" do
          "cdr" `parsesManySFrmTo` (SFrm Cdr)
        it "parses cons" do
          "cons" `parsesManySFrmTo` (SFrm Cons)
        it "parses define" do
          "define" `parsesManySFrmTo` (SFrm Def)
        it "parses equal?" do
          "equal?" `parsesManySFrmTo` (SFrm IsEq)
        it "parses lambda" do
          "lambda" `parsesManySFrmTo` (SFrm Lambda)
        it "parses quote" do
          "quote" `parsesManySFrmTo` (SFrm Quote)
        it "parses if" do
          "if" `parsesManySFrmTo` (SFrm If)
    describe "Parser.parseOne" do
      describe "integer" do
        it "parses a integer" $ do
          quickCheck prop_parseOne_Integer
      describe "float" do
        it "parses a float" $ do
          quickCheck prop_parseOne_Float
      describe "symbol" do
        it "parses a symbol" $ do
          quickCheck prop_parseOne_Sym
      describe "list" do
        it "parses a list" $ do
          quickCheck prop_parseOne_Lst
      describe "special forms" do
        it "parses atom?" do
          "atom?" `parsesOneSFrmTo` (SFrm IsAtm)
        it "parses car" do
          "car" `parsesOneSFrmTo` (SFrm Car)
        it "parses cdr" do
          "cdr" `parsesOneSFrmTo` (SFrm Cdr)
        it "parses cons" do
          "cons" `parsesOneSFrmTo` (SFrm Cons)
        it "parses define" do
          "define" `parsesOneSFrmTo` (SFrm Def)
        it "parses equal?" do
          "equal?" `parsesOneSFrmTo` (SFrm IsEq)
        it "parses lambda" do
          "lambda" `parsesOneSFrmTo` (SFrm Lambda)
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

parsesManySFrmTo :: String -> Expr -> Aff Unit
parsesManySFrmTo = parsesSFrm parseManySucceedsWith

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

parseManySucceedsWith :: String -> ExprAnn -> Aff Unit
parseManySucceedsWith str expected =
  case parseMany str of
    Left err -> fail $ show err
    Right actual -> actual `shouldEqual` (pure expected)

newtype ArbLst = ArbLst String
derive instance eqArbLst :: Eq ArbLst

instance arbitraryArbLst :: Arbitrary ArbLst where
  arbitrary = ArbLst <$> genLst

prop_parseOne_Lst :: ArbLst -> Boolean
prop_parseOne_Lst (ArbLst lst) =
  case parseOne lst of
    Right (ExprAnn (Lst _) _) -> true
    _ -> false

prop_parseMany_Lst :: ArbLst -> Boolean
prop_parseMany_Lst (ArbLst lst) =
  case parseMany lst of
    Right (L.Cons (ExprAnn (Lst _) _) _) -> true
    _ -> false

newtype ArbFloat = ArbFloat String
derive instance eqArbFloat :: Eq ArbFloat

instance arbitraryArbFloat :: Arbitrary ArbFloat where
  arbitrary = do
    float <- arbitrary :: Gen Number
    pure $ ArbFloat (show float)

prop_parseOne_Float :: ArbFloat -> Boolean
prop_parseOne_Float (ArbFloat float) =
  case parseOne float of
    Right (ExprAnn (Float _) _) -> true
    _ -> false

prop_parseMany_Float :: ArbFloat -> Boolean
prop_parseMany_Float (ArbFloat float) =
  case parseMany float of
    Right (L.Cons (ExprAnn (Float _) _) _) -> true
    _ -> false

newtype ArbInteger = ArbInteger String
derive instance eqArbInteger :: Eq ArbInteger

instance arbitraryArbInteger :: Arbitrary ArbInteger where
  arbitrary = do
    int <- arbitrary :: Gen Int
    pure $ ArbInteger (show int)

prop_parseOne_Integer :: ArbInteger -> Boolean
prop_parseOne_Integer (ArbInteger int) =
  case parseOne int of
    Right (ExprAnn (Integer _) _) -> true
    _ -> false

prop_parseMany_Integer :: ArbInteger -> Boolean
prop_parseMany_Integer (ArbInteger int) =
  case parseMany int of
    Right (L.Cons (ExprAnn (Integer _) _) _) -> true
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

prop_parseMany_Sym :: ArbSym -> Boolean
prop_parseMany_Sym (ArbSym sym) =
  case parseMany sym of
    Right (L.Cons (ExprAnn expr _) _) -> expr == Sym sym
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
  where sFrms = ["::", "=", "==", "atom?", "first", "lambda", "if", "quote", "rest"]
