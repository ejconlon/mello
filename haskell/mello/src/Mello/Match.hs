{-# LANGUAGE UndecidableInstances #-}

module Mello.Match
  ( MatchErr (..)
  , LocMatchErr (..)
  , MatchT
  , MatchM
  , runMatchT
  , runMatchM
  , SeqMatchT
  , SeqMatchM
  , annoM
  , memoM
  , embedM
  , matchM
  , listM
  , lookM
  , elemM
  , restM
  , repeatM
  , remainingM
  , altM
  , anySymM
  , symM
  , anyIntM
  , intM
  , anySciM
  , sciM
  , anyStrM
  , strM
  , anyCharM
  , charM
  , anyAtomM
  , quoteM
  , unquoteM
  , docM
  , MatchSexp (..)
  , fromSexpT
  , fromSexp
  , fromAnnoSexpT
  , fromAnnoSexp
  , proxyM
  )
where

import Bowtie (Anno (..), Memo (..), mkMemo, unMkMemo, pattern MemoP)
import Bowtie qualified as B
import Control.Exception (Exception (..))
import Control.Monad (ap, unless)
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.Identity (Identity (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..), ask, asks, local)
import Control.Monad.State (MonadState (..), StateT, runStateT)
import Control.Monad.Trans (MonadTrans (..))
import Data.Proxy (Proxy)
import Data.Scientific (Scientific)
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Foldable (toList)
import Data.List (intercalate, nub)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Mello.Syntax (Atom (..), AtomType (..), Brace, Doc, Sexp (..), SexpF (..), SexpType (..), Sym (..))

data MatchErr e r
  = MatchErrType !SexpType
  | MatchErrTypeAtom
  | MatchErrTypeQuote
  | MatchErrTypeUnquote
  | MatchErrTypeDoc
  | MatchErrNotEq !Atom
  | MatchErrListElem !Int
  | MatchErrListRem
  | MatchErrAlt !(Seq (Text, r))
  | MatchErrEmbed !e
  deriving stock (Eq, Ord, Show)

instance (Typeable e, Show e, Typeable r, Exception r) => Exception (MatchErr e r) where
  displayException = \case
    MatchErrType st -> "expected " <> displaySexpType st
    MatchErrTypeAtom -> "expected atom"
    MatchErrTypeQuote -> "expected quote"
    MatchErrTypeUnquote -> "expected unquote"
    MatchErrTypeDoc -> "expected doc"
    MatchErrNotEq a -> "expected " <> show a
    MatchErrListElem i -> "in list element " <> show i
    MatchErrListRem -> "unexpected remaining list elements"
    MatchErrAlt alts ->
      let reasons = nub (map (displayException . snd) (toList alts))
       in "no matching alternative: " <> intercalate "; " reasons
    MatchErrEmbed e -> show e

newtype LocMatchErr e k = LocMatchErr
  { unLocMatchErr :: Anno k (MatchErr e (LocMatchErr e k))
  }
  deriving stock (Show)
  deriving newtype (Eq, Ord)

instance (Typeable e, Show e, Typeable k, Show k) => Exception (LocMatchErr e k) where
  displayException (LocMatchErr (Anno _ err)) = displayException err

displaySexpType :: SexpType -> String
displaySexpType = \case
  SexpTypeAtom AtomTypeSym -> "symbol"
  SexpTypeAtom AtomTypeInt -> "integer"
  SexpTypeAtom AtomTypeSci -> "number"
  SexpTypeAtom AtomTypeStr -> "string"
  SexpTypeAtom AtomTypeChar -> "char"
  SexpTypeList _ -> "list"
  SexpTypeQuote -> "quote"
  SexpTypeUnquote -> "unquote"
  SexpTypeDoc -> "doc"

newtype MatchT e k m a = MatchT {unMatchT :: ReaderT (Memo SexpF k) (ExceptT (LocMatchErr e k) m) a}
  deriving newtype (Functor, Applicative, Monad)

type MatchM e k = MatchT e k Identity

instance MonadTrans (MatchT e k) where
  lift = MatchT . lift . lift

unlift :: (Monad m) => (Memo SexpF k -> m (Either (LocMatchErr e k) a)) -> MatchT e k m a
unlift f = MatchT $ do
  s <- ask
  ea <- lift (lift (f s))
  either throwError pure ea

instance (MonadReader r m) => MonadReader r (MatchT e k m) where
  ask = lift ask
  local f m = unlift (local f . runMatchT m)

instance (MonadState s m) => MonadState s (MatchT e k m) where
  get = lift get
  put = lift . put
  state = lift . state

instance (MonadError x m) => MonadError x (MatchT e k m) where
  throwError = lift . throwError
  catchError m f = unlift (\s -> catchError (runMatchT m s) (flip runMatchT s . f))

runMatchT :: MatchT e k m a -> Memo SexpF k -> m (Either (LocMatchErr e k) a)
runMatchT m r = runExceptT (runReaderT (unMatchT m) r)

runMatchM :: MatchM e k a -> Memo SexpF k -> Either (LocMatchErr e k) a
runMatchM m r = runIdentity (runMatchT m r)

data SeqMatchT e k m a where
  SeqMatchPure :: a -> SeqMatchT e k m a
  SeqMatchEmbed :: MatchT e k m (SeqMatchT e k m a) -> SeqMatchT e k m a
  SeqMatchElem :: MatchT e k m x -> (x -> SeqMatchT e k m a) -> SeqMatchT e k m a
  SeqMatchRepeat :: SeqMatchT e k m x -> (Seq x -> SeqMatchT e k m a) -> SeqMatchT e k m a
  SeqMatchRemaining :: (Int -> SeqMatchT e k m a) -> SeqMatchT e k m a

type SeqMatchM e k = SeqMatchT e k Identity

instance (Functor m) => Functor (SeqMatchT e k m) where
  fmap f = go
   where
    go = \case
      SeqMatchPure a -> SeqMatchPure (f a)
      SeqMatchEmbed mr -> SeqMatchEmbed (fmap go mr)
      SeqMatchElem mx k -> SeqMatchElem mx (go . k)
      SeqMatchRepeat mx k -> SeqMatchRepeat mx (go . k)
      SeqMatchRemaining k -> SeqMatchRemaining (go . k)

instance (Monad m) => Applicative (SeqMatchT e k m) where
  pure = SeqMatchPure
  (<*>) = ap

instance (Monad m) => Monad (SeqMatchT e k m) where
  return = pure
  r0 >>= f = go r0
   where
    go = \case
      SeqMatchPure a -> f a
      SeqMatchEmbed mr -> SeqMatchEmbed (fmap go mr)
      SeqMatchElem mx k -> SeqMatchElem mx (go . k)
      SeqMatchRepeat mx k -> SeqMatchRepeat mx (go . k)
      SeqMatchRemaining k -> SeqMatchRemaining (go . k)

annoM :: (Monad m) => MatchT e k m a -> MatchT e k m (Anno k a)
annoM m = MatchT (asks (Anno . B.memoKey)) <*> m

memoM :: (Monad m) => MatchT e k m (f (Memo f k)) -> MatchT e k m (Memo f k)
memoM m = MatchT (asks (MemoP . B.memoKey)) <*> m

errM :: (Monad m) => MatchErr e (LocMatchErr e k) -> MatchT e k m a
errM e = do
  s <- MatchT ask
  MatchT (throwError (LocMatchErr (Anno (B.memoKey s) e)))

embedM :: (Monad m) => e -> MatchT e k m a
embedM = errM . MatchErrEmbed

matchM :: (Monad m) => (SexpF (Memo SexpF k) -> Either (MatchErr e (LocMatchErr e k)) a) -> MatchT e k m a
matchM f = do
  s <- MatchT ask
  case f (B.memoVal s) of
    Left e -> MatchT (throwError (LocMatchErr (Anno (B.memoKey s) e)))
    Right a -> pure a

data S k = S !Int !(Seq (Memo SexpF k))
  deriving stock (Eq, Ord, Show)

listM :: (Monad m) => Brace -> SeqMatchT e k m a -> MatchT e k m a
listM = listFromM 0

-- heuper for listFromM, but needs type sig
goSeqX :: (Monad m) => SeqMatchT e k m a -> StateT (S k) (MatchT e k m) a
goSeqX = \case
  SeqMatchPure a -> pure a
  SeqMatchEmbed mr -> lift mr >>= goSeqX
  SeqMatchElem mx k -> do
    S i cs <- get
    let i' = i + 1
    case cs of
      Empty -> lift (errM (MatchErrListElem i'))
      c :<| cs' -> do
        put (S i' cs')
        x <- lift (MatchT (local (const c) (unMatchT mx)))
        goSeqX (k x)
  SeqMatchRepeat mx k -> goRepeatX mx k
  SeqMatchRemaining k -> do
    S _ cs <- get
    goSeqX (k (Seq.length cs))

-- TODO Better error for failure on repeat?
-- helper for listFromM, but needs type sig
goRepeatX :: (Monad m) => SeqMatchT e k m x -> (Seq x -> SeqMatchT e k m a) -> StateT (S k) (MatchT e k m) a
goRepeatX mx k = go Empty
 where
  go !acc = do
    S _ cs <- get
    case cs of
      Empty -> goSeqX (k acc)
      _ -> do
        x <- goSeqX mx
        go (acc :|> x)

listFromM :: (Monad m) => Int -> Brace -> SeqMatchT e k m a -> MatchT e k m a
listFromM i0 b0 r = goStart
 where
  goStart = do
    s <- MatchT ask
    case B.memoVal s of
      SexpListF b cs0 | b == b0 -> do
        let s0 = S i0 (Seq.drop i0 cs0)
        (a, S _ cs1) <- runStateT (goSeqX r) s0
        case cs1 of
          Empty -> pure a
          _ -> errM MatchErrListRem
      _ -> errM (MatchErrType (SexpTypeList b0))

elemM :: MatchT e k m a -> SeqMatchT e k m a
elemM = (`SeqMatchElem` SeqMatchPure)

restM :: MatchT e k m a -> SeqMatchT e k m (Seq a)
restM = repeatM . elemM

repeatM :: SeqMatchT e k m a -> SeqMatchT e k m (Seq a)
repeatM = (`SeqMatchRepeat` SeqMatchPure)

remainingM :: SeqMatchT e k m Int
remainingM = SeqMatchRemaining SeqMatchPure

altM :: (Monad m) => [(Text, MatchT e k m a)] -> MatchT e k m a
altM = go Empty
 where
  go !acc = \case
    [] -> errM (MatchErrAlt acc)
    (l, m) : ms -> do
      s <- MatchT ask
      res <- lift (runMatchT m s)
      case res of
        Right a -> pure a
        Left e -> go (acc :|> (l, e)) ms

lookM :: (Monad m) => Brace -> [(Text, MatchT e k m (), SeqMatchT e k m a)] -> MatchT e k m a
lookM b0 as0 = goRoot
 where
  goRoot = do
    s <- MatchT ask
    case B.memoVal s of
      SexpListF b cs0 | b == b0 ->
        case cs0 of
          Empty -> errM (MatchErrListElem 0)
          hd :<| _ -> goAlt hd Empty as0
      _ -> errM (MatchErrType (SexpTypeList b0))
  goAlt hd !acc = \case
    [] -> errM (MatchErrAlt acc)
    (l, m, r) : as -> do
      resHd <- lift (runMatchT m hd)
      case resHd of
        Right _ -> do
          s <- MatchT ask
          resTl <- lift (runMatchT (listFromM 1 b0 r) s)
          case resTl of
            Right a -> pure a
            Left e -> goAlt hd (acc :|> (l, e)) as
        Left e -> goAlt hd (acc :|> (l, e)) as

anySymM :: (Monad m) => MatchT e k m Sym
anySymM = matchM $ \case
  SexpAtomF (AtomSym y) -> Right y
  _ -> Left (MatchErrType (SexpTypeAtom AtomTypeSym))

symM :: (Monad m) => Sym -> MatchT e k m ()
symM x =
  anySymM >>= \y ->
    unless (y == x) (errM (MatchErrNotEq (AtomSym x)))

anyIntM :: (Monad m) => MatchT e k m Integer
anyIntM = matchM $ \case
  SexpAtomF (AtomInt y) -> Right y
  _ -> Left (MatchErrType (SexpTypeAtom AtomTypeInt))

intM :: (Monad m) => Integer -> MatchT e k m ()
intM x =
  anyIntM >>= \y ->
    unless (y == x) (errM (MatchErrNotEq (AtomInt x)))

anySciM :: (Monad m) => MatchT e k m Scientific
anySciM = matchM $ \case
  SexpAtomF (AtomSci y) -> Right y
  _ -> Left (MatchErrType (SexpTypeAtom AtomTypeSci))

sciM :: (Monad m) => Scientific -> MatchT e k m ()
sciM x =
  anySciM >>= \y ->
    unless (y == x) (errM (MatchErrNotEq (AtomSci x)))

anyStrM :: (Monad m) => MatchT e k m Text
anyStrM = matchM $ \case
  SexpAtomF (AtomStr y) -> Right y
  _ -> Left (MatchErrType (SexpTypeAtom AtomTypeStr))

strM :: (Monad m) => Text -> MatchT e k m ()
strM x =
  anyStrM >>= \y ->
    unless (y == x) (errM (MatchErrNotEq (AtomStr x)))

anyCharM :: (Monad m) => MatchT e k m Char
anyCharM = matchM $ \case
  SexpAtomF (AtomChar y) -> Right y
  _ -> Left (MatchErrType (SexpTypeAtom AtomTypeChar))

charM :: (Monad m) => Char -> MatchT e k m ()
charM x =
  anyCharM >>= \y ->
    unless (y == x) (errM (MatchErrNotEq (AtomChar x)))

anyAtomM :: (Monad m) => MatchT e k m Atom
anyAtomM = matchM $ \case
  SexpAtomF a -> Right a
  _ -> Left MatchErrTypeAtom

quoteM :: (Monad m) => MatchT e k m (Memo SexpF k)
quoteM = matchM $ \case
  SexpQuoteF x -> Right x
  _ -> Left MatchErrTypeQuote

unquoteM :: (Monad m) => MatchT e k m (Memo SexpF k)
unquoteM = matchM $ \case
  SexpUnquoteF x -> Right x
  _ -> Left MatchErrTypeQuote

docM :: (Monad m) => MatchT e k m a -> MatchT e k m (Doc, a)
docM m = do
  (d, x) <- matchM $ \case
    SexpDocF d x -> Right (d, x)
    _ -> Left MatchErrTypeDoc
  a <- MatchT (local (const x) (unMatchT m))
  pure (d, a)

class (Monad m) => MatchSexp e k m a where
  matchSexp :: MatchT e k m a

instance (Monad m) => MatchSexp e k m Sexp where
  matchSexp = matchM (Right . Sexp . fmap unMkMemo)

instance (Monad m) => MatchSexp e k m (Memo SexpF k) where
  matchSexp = memoM (matchM Right)

instance (MatchSexp e k m s) => MatchSexp e k m (Anno k s) where
  matchSexp = annoM matchSexp

instance (Monad m) => MatchSexp e k m Atom where
  matchSexp = anyAtomM

instance (Monad m) => MatchSexp e k m Sym where
  matchSexp = anySymM

instance (Monad m) => MatchSexp e k m Integer where
  matchSexp = anyIntM

instance (Monad m) => MatchSexp e k m Scientific where
  matchSexp = anySciM

instance (Monad m) => MatchSexp e k m Text where
  matchSexp = anyStrM

instance (Monad m) => MatchSexp e k m Char where
  matchSexp = anyCharM

fromSexpT :: (MatchSexp e () m a) => Sexp -> m (Either (LocMatchErr e ()) a)
fromSexpT = runMatchT matchSexp . mkMemo (const ())

fromSexp :: (MatchSexp e () Identity a) => Sexp -> Either (LocMatchErr e ()) a
fromSexp = runIdentity . fromSexpT

fromAnnoSexpT :: (MatchSexp e k m a) => Memo SexpF k -> m (Either (LocMatchErr e k) a)
fromAnnoSexpT = runMatchT matchSexp

fromAnnoSexp :: (MatchSexp e k Identity a) => Memo SexpF k -> Either (LocMatchErr e k) a
fromAnnoSexp = runIdentity . fromAnnoSexpT

proxyM :: (MatchSexp e k m a) => Proxy a -> MatchT e k m a
proxyM = const matchSexp
