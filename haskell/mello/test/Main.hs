{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Bowtie (unMkMemo)
import Control.Exception (displayException)
import Data.List (isInfixOf)
import Data.Text (Text)
import Data.Void (Void)
import Mello.Match (MatchM, altM, anyIntM, anySymM, elemM, listM, runMatchM, symM)
import Mello.Parse (LocSpan, parseSexp, sexpParser)
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
import PropUnit (MonadTest, TestName, TestTree, testGroup, testUnit)
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
        let m = altM [("sym", anySymM >> pure ()), ("int", anyIntM >> pure ())]
        assertErrContains "sym:" (trySexp m "(x)")
        assertErrContains "int:" (trySexp m "(x)")
    , testUnit "alt: no match" $
        assertErrContains "no match" (trySexp (altM [("sym", anySymM >> pure ())]) "(x)")
    ]

main :: IO ()
main =
  daytripperMain $ \_ ->
    testGroup
      "mello"
      [ testParsing
      , testDisplayException
      ]
