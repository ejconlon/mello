{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Mello.Parse
  ( OffsetSpan
  , OffsetSexp
  , Loc (..)
  , LocSpan
  , LocSexp
  , ParseErr (..)
  , sexpParser
   , parseSexp
   , parseSexpDetailed
   , parseSexpI
   , parseSexpDetailedI
   )
where

import Bowtie (Memo, pattern MemoP)
import Control.Exception (Exception (..))
import Control.Monad (guard, unless, void)
import Data.Bifunctor (first)
import Data.Char (isDigit, isSpace)
import Data.Hashable (Hashable)
import Data.Sequence (Seq (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import GHC.Generics (Generic)
import Looksee (Err, ParserT, Span (..), errSpan)
import Looksee qualified as L
import Mello.Syntax (Atom (..), Doc (..), SexpF (..), Sym (..))
import Mello.Recognize (RecogErr (..), recognizeSexpText)
import Mello.Text
  ( Brace
  , closeBraceChar
  , isAtomStart
  , isCharStart
  , isListStart
  , isQuoteStart
  , isStringStart
  , isSymCont
  , isSymStart
  , isUnquoteStart
  , openBraceChar
  )

-- Generic parser combinators

guard1P :: (Monad m) => (Char -> Bool) -> ParserT e m ()
guard1P f = L.headP >>= guard . f

cons1P :: (Monad m) => (Char -> Bool) -> (Char -> Bool) -> ParserT e m Text
cons1P f g = liftA2 T.cons (L.headP >>= \c -> c <$ guard (f c)) (L.takeWhileP g)

commitSameP :: (Monad m) => [ParserT e m a] -> ParserT e m a
commitSameP = L.commitP . fmap (\p -> (void p, p))

explainEmptyP :: (Monad m) => Text -> ParserT e m a -> ParserT e m a
explainEmptyP msg = L.explainP $ \case
  L.ReasonEmpty -> Just (msg, True)
  _ -> Nothing

-- The final recursive types

type OffsetSpan = Span Int

type OffsetSexp = Memo SexpF OffsetSpan

data Loc = Loc
  { locLine :: !Int
  , locCol :: !Int
  , locOffset :: !Int
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

type LocSpan = Span Loc

type LocSexp = Memo SexpF LocSpan

data ParseErr
  = -- | An opening delimiter was not closed before EOF.
    ParseErrUnclosedList !Brace !LocSpan
  | -- | A closing delimiter appeared with no matching opener.
    ParseErrUnexpectedClose !Brace !LocSpan
  | -- | A closing delimiter did not match the most recent opener.
    ParseErrMismatchedBrace !Brace !LocSpan !Brace !LocSpan
  | -- | A generic Looksee parse error, paired with its span converted to
    -- Mello locations. This constructor intentionally exposes both values:
    -- clients can render or inspect the original 'Err' while using 'LocSpan'
    -- directly for Mello-style source annotations.
    ParseErrLooksee !LocSpan !(Err Void)
  deriving stock (Show)

instance Exception ParseErr where
  displayException = \case
    ParseErrUnclosedList brace _ -> "unclosed list; expected '" <> [closeBraceChar brace] <> "' before end of input"
    ParseErrUnexpectedClose brace _ -> "unexpected closing delimiter '" <> [closeBraceChar brace] <> "'"
    ParseErrMismatchedBrace expected _ actual _ -> "mismatched closing delimiter; expected '" <> [closeBraceChar expected] <> "' but got '" <> [closeBraceChar actual] <> "'"
    ParseErrLooksee _ err -> displayException err

-- Specific parsers

docStartP :: (Monad m) => ParserT e m ()
docStartP = L.textP_ ";|"

commentStartP :: (Monad m) => ParserT e m ()
commentStartP = L.charP_ ';'

spaceNP :: (Monad m) => Int -> ParserT e m Int
spaceNP !acc = do
  mc <- L.lookP L.unconsP
  case mc of
    Just ';' -> do
      mds <- L.lookP (L.optP docStartP)
      case mds of
        Just _ -> pure acc
        Nothing -> L.dropWhileP (/= '\n') >>= spaceNP . (acc +)
    Just c | isSpace c -> L.dropWhileP isSpace >>= spaceNP . (acc +)
    _ -> pure acc

spaceP, space1P :: (Monad m) => ParserT e m ()
spaceP = void (spaceNP 0)
space1P = do
  acc <- spaceNP 0
  unless (acc > 0) L.space1P -- Use this to fail

stripP, stripEndP :: (Monad m) => ParserT e m a -> ParserT e m a
stripP p = spaceP *> p <* spaceP
stripEndP p = p <* spaceP

symP :: (Monad m) => ParserT e m Sym
symP = fmap Sym (cons1P isSymStart isSymCont)

charLitP :: (Monad m) => ParserT e m Char
charLitP = L.charP_ '\'' *> L.headP <* L.charP_ '\''

stringLitP :: (Monad m) => ParserT e m Text
stringLitP = L.strP '"'

openBraceP :: (Monad m) => ParserT e m Brace
openBraceP = commitSameP (fmap (\b -> b <$ L.charP_ (openBraceChar b)) [minBound .. maxBound])

closeBraceP :: (Monad m) => Brace -> ParserT e m ()
closeBraceP = L.charP_ . closeBraceChar

docLinesP :: (Monad m) => ParserT e m Doc
docLinesP = go True Empty
 where
  lineStartP isFirst = if isFirst then docStartP else commentStartP
  lineP isFirst = do
    lineStartP isFirst
    lin <- L.takeWhileP (/= '\n')
    L.charP_ '\n'
    pure lin
  go !isFirst !acc = do
    mx <- L.lookP (L.optP (lineStartP isFirst))
    case mx of
      Nothing -> pure (Doc acc)
      Just _ -> do
        lin <- lineP isFirst
        go False (acc :|> lin)

-- | A parser for S-expressions
sexpParser :: (Monad m) => ParserT e m OffsetSexp
sexpParser = stripP rootP
 where
  rootP =
    explainEmptyP "Not a recognizable Sexp" $
      L.spanAroundP MemoP $
        L.commitP
          [ (guard1P isListStart, L.labelP "list" listP)
          , (guard1P isQuoteStart, L.labelP "quote" quoteP)
          , (guard1P isUnquoteStart, L.labelP "unquote" unquoteP)
          , (guard1P isAtomStart, L.labelP "atom" atomP)
          , (docStartP, L.labelP "doc" docP)
          ]
  listP = do
    b <- stripEndP openBraceP
    ss <- stripEndP (L.sepByP space1P rootP)
    closeBraceP b
    pure (SexpListF b ss)
  quoteP = L.charP_ '`' *> fmap SexpQuoteF rootP
  unquoteP = L.charP_ ',' *> fmap SexpUnquoteF rootP
  guardNumStartP = do
    c <- L.headP
    if isDigit c
      then pure ()
      else do
        guard (c == '-')
        void (L.headP >>= guard . isDigit)
  atomP =
    SexpAtomF
      <$> L.commitP
        [ (guardNumStartP, L.labelP "num" (fmap (either AtomInt AtomSci) L.numP))
        , (guard1P isSymStart, L.labelP "sym" (fmap AtomSym symP))
        , (guard1P isStringStart, L.labelP "str" (fmap AtomStr stringLitP))
        , (guard1P isCharStart, L.labelP "char" (fmap AtomChar charLitP))
        ]
  docP = do
    doc <- docLinesP
    fmap (SexpDocF doc) rootP

parseSexp :: Text -> Either (Err Void) LocSexp
parseSexp txt = do
  sexp <- L.parse sexpParser txt
  let v = L.calculateLineCol txt
      mkLoc o = let (l, c) = L.lookupLineCol o v in Loc l c o
  pure (fmap (fmap mkLoc) sexp)

-- | Parse an s-expression with Mello-domain diagnostics.
--
-- Delimiter errors are classified before running the full parser. Generic
-- parser failures keep the original Looksee error and include a converted
-- Mello location span via 'ParseErrLooksee'.
parseSexpDetailed :: Text -> Either ParseErr LocSexp
parseSexpDetailed txt = do
  first (parseErrRecognize txt) (recognizeSexpText txt)
  first (parseErrLooksee txt) (parseSexp txt)

parseErrLooksee :: Text -> Err Void -> ParseErr
parseErrLooksee txt err = ParseErrLooksee (fmap (locAt txt) (errSpan err)) err

parseErrRecognize :: Text -> RecogErr -> ParseErr
parseErrRecognize txt = \case
  RecogErrUnclosedList brace openOffset eofOffset -> ParseErrUnclosedList brace (Span (locAt txt openOffset) (locAt txt eofOffset))
  RecogErrUnexpectedClose brace closeOffset -> ParseErrUnexpectedClose brace (offsetSpan txt closeOffset)
  RecogErrMismatchedBrace expected openOffset actual closeOffset -> ParseErrMismatchedBrace expected (offsetSpan txt openOffset) actual (offsetSpan txt closeOffset)

offsetSpan :: Text -> Int -> LocSpan
offsetSpan txt offset = Span (locAt txt offset) (locAt txt (offset + 1))

locAt :: Text -> Int -> Loc
locAt txt offset =
  let lookupTable = L.calculateLineCol txt
      (line, col) = L.lookupLineCol offset lookupTable
   in Loc line col offset

parseSexpI :: Text -> IO (Either (Err Void) LocSexp)
parseSexpI txt = do
  let ea = parseSexp txt
  case ea of
    Left e -> L.printE "<interactive>" txt e
    Right _ -> pure ()
  pure ea

parseSexpDetailedI :: Text -> IO (Either ParseErr LocSexp)
parseSexpDetailedI txt = do
  let ea = parseSexpDetailed txt
  case ea of
    Left (ParseErrLooksee _ err) -> L.printE "<interactive>" txt err
    Left err -> putStrLn (displayException err)
    Right _ -> pure ()
  pure ea
