{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Bowtie (unMkMemo)
import Data.Text (Text)
import Data.Void (Void)
import Mello.Parse (sexpParser)
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
import PropUnit (MonadTest, TestName, TestTree, testGroup)
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

main :: IO ()
main =
  daytripperMain $ \_ ->
    testGroup
      "mello"
      [ testParsing
      ]
