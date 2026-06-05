-- | Lightweight lexical delimiter recognition for Mello s-expressions.
--
-- This module does not parse s-expressions. It only checks delimiter balance
-- while respecting strings, chars, comments, and escapes. Offsets are returned
-- directly so callers can convert them to their preferred source-location type.
module Mello.Recognize
  ( RecogErr (..)
  , sexpRecognizer
  , recognizeSexpText
  )
where

import Control.Foldl (Fold (..))
import Control.Foldl qualified as Foldl
import Control.Monad.Except (ExceptT (..), MonadError (..), runExceptT)
import Control.Monad.State.Strict (State, gets, modify', runState)
import Data.Text (Text)
import Data.Text qualified as T
import Mello.Text (Brace, readCloseBrace, readOpenBrace)

data X e s = X !(Maybe e) !s

foldUntilErr :: (a -> ExceptT e (State s) ()) -> s -> (Maybe e -> s -> b) -> Fold a b
foldUntilErr step initial extract = Fold step' initial' extract'
 where
  step' x@(X me s) a =
    case me of
      Just _ -> x
      Nothing ->
        let (ea, s') = runState (runExceptT (step a)) s
        in  case ea of
              Left e -> X (Just e) s'
              Right _ -> X Nothing s'
  initial' = X Nothing initial
  extract' (X me s) = extract me s

data RecogErr
  = -- | An opener was not closed before EOF: brace, opener offset, EOF offset.
    RecogErrUnclosedList !Brace !Int !Int
  | -- | A closer appeared without a matching opener: actual brace, closer offset.
    RecogErrUnexpectedClose !Brace !Int
  | -- | A closer did not match the most recent opener: expected brace, opener offset, actual brace, closer offset.
    RecogErrMismatchedBrace !Brace !Int !Brace !Int
  deriving stock (Eq, Ord, Show)

data RecogState = RecogState
  { rsOffset :: !Int
  , rsMode :: !RecogMode
  , rsStack :: ![OpenList]
  }
  deriving stock (Eq, Ord, Show)

initRecogState :: RecogState
initRecogState = RecogState 0 RecogModeDefault []

type RecogM = ExceptT RecogErr (State RecogState)

data RecogMode
  = RecogModeDefault
  | RecogModeString
  | RecogModeChar
  | RecogModeComment
  | RecogModeEscape !RecogMode
  deriving stock (Eq, Ord, Show)

data OpenList = OpenList !Brace !Int
  deriving stock (Eq, Ord, Show)

data CharCase
  = CharCaseNewline
  | CharCaseDoubleQuote
  | CharCaseSingleQuote
  | CharCaseOpenComment
  | CharCaseSlashEsc
  | CharCaseOpenBrace !Brace
  | CharCaseCloseBrace !Brace
  deriving stock (Eq, Ord, Show)

readCharCase :: Char -> Maybe CharCase
readCharCase c =
  if
    | c == '\n' -> Just CharCaseNewline
    | c == '"' -> Just CharCaseDoubleQuote
    | c == '\'' -> Just CharCaseSingleQuote
    | c == ';' -> Just CharCaseOpenComment
    | c == '\\' -> Just CharCaseSlashEsc
    | otherwise ->
        case readOpenBrace c of
          Just b -> Just (CharCaseOpenBrace b)
          Nothing -> case readCloseBrace c of
            Just b -> Just (CharCaseCloseBrace b)
            Nothing -> Nothing

stepR :: Char -> RecogM ()
stepR c = go <* incOffset
 where
  go = do
    mode <- gets rsMode
    case mode of
      RecogModeDefault -> goDefault
      RecogModeString -> goString
      RecogModeChar -> goChar
      RecogModeComment -> goComment
      RecogModeEscape mode' -> setMode mode'
  goString = case readCharCase c of
    Just CharCaseDoubleQuote -> setMode RecogModeDefault
    Just CharCaseSlashEsc -> setMode (RecogModeEscape RecogModeString)
    _ -> pure ()
  goChar = case readCharCase c of
    Just CharCaseSingleQuote -> setMode RecogModeDefault
    Just CharCaseSlashEsc -> setMode (RecogModeEscape RecogModeChar)
    _ -> pure ()
  goComment = case readCharCase c of
    Just CharCaseNewline -> setMode RecogModeDefault
    _ -> pure ()
  goDefault = case readCharCase c of
    Just CharCaseDoubleQuote -> setMode RecogModeString
    Just CharCaseSingleQuote -> setMode RecogModeChar
    Just CharCaseOpenComment -> setMode RecogModeComment
    Just (CharCaseOpenBrace b) -> pushOpen b
    Just (CharCaseCloseBrace b) ->
      peekOpen >>= \case
        Just (OpenList b0 _) | b == b0 -> popOpen
        Just (OpenList b0 offset) -> throwAt (RecogErrMismatchedBrace b0 offset b)
        Nothing -> throwAt (RecogErrUnexpectedClose b)
    _ -> pure ()
  incOffset = modify' (\s -> s {rsOffset = rsOffset s + 1})
  setMode mode = modify' (\s -> s {rsMode = mode})
  pushOpen b = do
    offset <- gets rsOffset
    modify' (\s -> s {rsStack = OpenList b offset : rsStack s})
  popOpen = modify' $ \s ->
    case rsStack s of
      [] -> s
      _ : t -> s {rsStack = t}
  peekOpen = gets $ \s ->
    case rsStack s of
      [] -> Nothing
      h : _ -> Just h
  throwAt f = gets rsOffset >>= throwError . f

extractR :: Maybe RecogErr -> RecogState -> Either RecogErr ()
extractR me s = case me of
  Just err -> Left err
  Nothing -> case rsStack s of
    [] -> Right ()
    OpenList brace openOffset : _ -> Left (RecogErrUnclosedList brace openOffset (rsOffset s))

sexpRecognizer :: Fold Char (Either RecogErr ())
sexpRecognizer = foldUntilErr stepR initRecogState extractR

-- | Recognize delimiter balance in a complete text value.
recognizeSexpText :: Text -> Either RecogErr ()
recognizeSexpText = Foldl.fold sexpRecognizer . T.unpack
