{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Bowtie (unMkMemo)
import Control.Exception (displayException)
import Control.Monad (void)
import Data.List (isInfixOf)
import Data.Text (Text)
import Data.Void (Void)
import Looksee (Span (..))
import Mello.Match (MatchM, altM, anyIntM, anySymM, elemM, expectAnyInt, expectAnySym, listM, runMatchM, symM)
import Mello.Parse (Loc (..), LocSpan, ParseErr (..), parseSexp, parseSexpDetailed, sexpParser)
import Mello.Recognize (RecogErr (..), recognizeSexpText)
import Mello.Syntax
  ( Atom (..)
  , Brace (..)
  , Doc (..)
  , Sexp
  , pattern SexpAtom
  , pattern SexpDoc
  , pattern SexpList
  , pattern SexpQuote
  , pattern SexpUnquote
  )
import PropUnit (Gen, MonadTest, TestLimit, TestName, TestTree, testGroup, testUnit)
import PropUnit qualified as PU
import PropUnit.Hedgehog.Gen qualified as Gen
import PropUnit.Hedgehog.Range qualified as Range
import Test.Daytripper (daytripperMain, mkUnitRT, testRT)
import Test.Looksee.Trip (ExpectP, cmpEq, expectParsePretty, expectRendered)

expectParseSexp :: (MonadTest m) => ExpectP Void m Sexp
expectParseSexp = expectParsePretty (fmap unMkMemo sexpParser) cmpEq

parseCase :: TestName -> Sexp -> TestTree
parseCase n = testRT undefined . mkUnitRT n expectParseSexp

parseCaseAs :: TestName -> Text -> Sexp -> TestTree
parseCaseAs n t = testRT undefined . mkUnitRT n (expectRendered t expectParseSexp)

testParsing :: TestTree
testParsing =
  testGroup
    "parsing"
    [ parseCaseAs "atom int" "1" (SexpAtom (AtomInt 1))
    , parseCaseAs "atom str" "\"abc\"" (SexpAtom (AtomStr "abc"))
    , parseCaseAs "atom char" "'x'" (SexpAtom (AtomChar 'x'))
    , parseCaseAs "atom sym" "xyz" (SexpAtom (AtomSym "xyz"))
    , parseCaseAs "atom sci" "3.14" (SexpAtom (AtomSci 3.14))
    , parseCaseAs "quote" "`1" (SexpQuote 1)
    , parseCaseAs "unquote" ",1" (SexpUnquote 1)
    , parseCaseAs "list" "(1 2)" (SexpList BraceParen [1, 2])
    , parseCaseAs "doc" ";|X\n;Y\n1" (SexpDoc (Doc ["X", "Y"]) 1)
    , parseCaseAs "atom int neg" "-1" (SexpAtom (AtomInt (-1)))
    , parseCaseAs "atom sym special 1" "_y-3z" (SexpAtom (AtomSym "_y-3z"))
    , parseCaseAs "atom sym special 2" ">=" (SexpAtom (AtomSym ">="))
    , parseCaseAs "atom sym special 3" "<=" (SexpAtom (AtomSym "<="))
    , parseCaseAs "atom sym special 4" "=>" (SexpAtom (AtomSym "=>"))
    , parseCaseAs "atom sym special 5" "->" (SexpAtom (AtomSym "->"))
    , parseCaseAs "atom sym special 6" ":x" (SexpAtom (AtomSym ":x"))
    ]

testParseSexpDetailed :: TestTree
testParseSexpDetailed =
  testGroup
    "parseSexpDetailed"
     [ testUnit "success" $ do
        case parseSexpDetailed "(1 2)" of
          Left err -> fail ("expected parse success, got: " <> displayException err)
          Right _ -> pure ()
    , testUnit "ignores delimiters in strings" $ do
        case parseSexpDetailed "(\"[}\" 1)" of
          Left err -> fail ("expected parse success, got: " <> displayException err)
          Right _ -> pure ()
    , testUnit "ignores delimiters in chars" $ do
        case parseSexpDetailed "(']' 1)" of
          Left err -> fail ("expected parse success, got: " <> displayException err)
          Right _ -> pure ()
    , testUnit "ignores delimiters in comments" $ do
        case parseSexpDetailed "(; ]\n 1)" of
          Left err -> fail ("expected parse success, got: " <> displayException err)
          Right _ -> pure ()
    , testUnit "ignores delimiters in docs" $ do
        case parseSexpDetailed ";| ]\n; }\n(1)" of
          Left err -> fail ("expected parse success, got: " <> displayException err)
          Right _ -> pure ()
    , testUnit "unclosed list" $ do
        case parseSexpDetailed "(1 2" of
          Left (ParseErrUnclosedList BraceParen (Span (Loc 0 0 0) (Loc 0 3 4))) -> pure ()
          other -> fail ("unexpected result: " <> show other)
    , testUnit "multiline unclosed list" $ do
        case parseSexpDetailed "(1\n 2" of
          Left (ParseErrUnclosedList BraceParen (Span (Loc 0 0 0) (Loc 1 1 5))) -> pure ()
          other -> fail ("unexpected result: " <> show other)
    , testUnit "unexpected close" $ do
        case parseSexpDetailed ")" of
          Left (ParseErrUnexpectedClose BraceParen (Span (Loc 0 0 0) (Loc 0 0 1))) -> pure ()
          other -> fail ("unexpected result: " <> show other)
    , testUnit "mismatched brace" $ do
        case parseSexpDetailed "[1)" of
          Left (ParseErrMismatchedBrace BraceSquare (Span (Loc 0 0 0) (Loc 0 1 1)) BraceParen (Span (Loc 0 2 2) (Loc 0 2 3))) -> pure ()
          other -> fail ("unexpected result: " <> show other)
    , testUnit "multiline mismatched brace" $ do
        case parseSexpDetailed "[1\n )" of
          Left (ParseErrMismatchedBrace BraceSquare (Span (Loc 0 0 0) (Loc 0 1 1)) BraceParen (Span (Loc 1 1 4) (Loc 1 1 5))) -> pure ()
          other -> fail ("unexpected result: " <> show other)
    , testUnit "generic parse errors carry source span" $ do
        case parseSexpDetailed "" of
          Left (ParseErrLooksee (Span (Loc 0 0 0) _) _) -> pure ()
          other -> fail ("unexpected result: " <> show other)
    ]

testRecognize :: TestLimit -> TestTree
testRecognize lim =
  testGroup
    "recognize"
    [ testUnit "success" $ do
        case recognizeSexpText "(1 [2] {3})" of
          Right () -> pure ()
          other -> fail ("unexpected result: " <> show other)
    , testUnit "quote does not crash" $ do
        case recognizeSexpText "`[1]" of
          Right () -> pure ()
          other -> fail ("unexpected result: " <> show other)
    , testUnit "unquote does not crash" $ do
        case recognizeSexpText ",[1]" of
          Right () -> pure ()
          other -> fail ("unexpected result: " <> show other)
    , testUnit "unclosed list" $ do
        case recognizeSexpText "(1\n 2" of
          Left (RecogErrUnclosedList BraceParen 0 5) -> pure ()
          other -> fail ("unexpected result: " <> show other)
    , testUnit "unexpected close" $ do
        case recognizeSexpText ")" of
          Left (RecogErrUnexpectedClose BraceParen 0) -> pure ()
          other -> fail ("unexpected result: " <> show other)
    , testUnit "mismatched brace" $ do
        case recognizeSexpText "`[1)" of
          Left (RecogErrMismatchedBrace BraceSquare 1 BraceParen 3) -> pure ()
          other -> fail ("unexpected result: " <> show other)
    , testUnit "ignores delimiters in strings chars and comments" $ do
        case recognizeSexpText "(\"[}\" ']' ; )\n 1)" of
          Right () -> pure ()
          other -> fail ("unexpected result: " <> show other)
    , PU.testProp "agrees with parser on generated balanced inputs" lim $ do
        input <- PU.forAll genBalancedInput
        case parseSexp input of
          Left err -> fail ("expected parse success for " <> show input <> ", got: " <> displayException err)
          Right _ -> pure ()
        case recognizeSexpText input of
          Left err -> fail ("expected recognize success for " <> show input <> ", got: " <> show err)
          Right () -> pure ()
    ]

genBalancedInput :: Gen Text
genBalancedInput = Gen.recursive Gen.choice [genAtom] [genList, genPrefix, genDoc]
 where
  genAtom =
    Gen.element
      ([ "1"
      , "abc"
      , "\"[not a delimiter]\""
      , "\"escaped \\\" ]\""
      , "']'"
      , "'x'"
      ] :: [Text])
  genList = do
    (open, close) <- Gen.element ([ ("(", ")"), ("[", "]"), ("{", "}") ] :: [(Text, Text)])
    body <- Gen.list (Range.constant 0 4) genBalancedInput
    pure (open <> foldMap (" " <>) body <> close)
  genPrefix = do
    prefix <- Gen.element (["`", ","] :: [Text])
    input <- genBalancedInput
    pure (prefix <> input)
  genDoc = do
    input <- genBalancedInput
    pure (";| doc ]\n; more }\n" <> input)

-- Match error display tests

trySexp :: MatchM Void LocSpan a -> Text -> Either String a
trySexp m t = do
  sexp <- either (Left . show) Right (parseSexp t)
  either (Left . displayException) Right (runMatchM @Void m sexp)

-- | Assert that matching fails and the error message contains the expected substring.
assertErrContains :: (MonadFail m) => String -> Either String a -> m ()
assertErrContains expected = \case
  Right _ -> fail "expected match to fail"
  Left err
    | expected `isInfixOf` err -> pure ()
    | otherwise -> fail ("error did not contain " <> show expected <> "\ngot: " <> err)

testDisplayException :: TestTree
testDisplayException =
  testGroup
    "displayException"
    [ testUnit "type mismatch: expected symbol" $
        assertErrContains "expected symbol" (trySexp anySymM "42")
    , testUnit "type mismatch: expected integer" $
        assertErrContains "expected integer" (trySexp anyIntM "foo")
    , testUnit "type mismatch: expected list" $
        assertErrContains "expected list" (trySexp (listM BraceParen (pure ())) "foo")
    , testUnit "list too short" $
        assertErrContains "list too short" (trySexp (listM BraceParen (elemM anySymM >> elemM anySymM)) "(x)")
    , testUnit "list too short: position" $
        assertErrContains "position 2" (trySexp (listM BraceParen (elemM anySymM >> elemM anySymM)) "(x)")
    , testUnit "unexpected remaining" $
        assertErrContains "unexpected remaining" (trySexp (listM BraceParen (elemM anySymM)) "(x y)")
    , testUnit "not equal: expected atom" $
        assertErrContains "expected foo" (trySexp (symM "foo") "bar")
    , testUnit "alt with labels" $ do
        let m = altM [("sym", expectAnySym, void anySymM), ("int", expectAnyInt, void anyIntM)]
        assertErrContains "symbol" (trySexp m "(x)")
        assertErrContains "integer" (trySexp m "(x)")
    , testUnit "alt: expected one of" $
        assertErrContains "expected one of" (trySexp (altM [("sym", expectAnySym, void anySymM)]) "(x)")
    ]

main :: IO ()
main =
  daytripperMain $ \lim ->
    testGroup
      "mello"
      [ testParsing
      , testParseSexpDetailed
      , testRecognize lim
      , testDisplayException
      ]
