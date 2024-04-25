{-# LANGUAGE UndecidableInstances #-}

module Mello.Print
  ( ToSexp (..)
  , toSexpDoc
  , toSexpText
  )
where

import Bowtie.Anno (Anno (..))
import Bowtie.Fix (Fix (..))
import Bowtie.Memo (Memo, memoVal)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as T
import Mello.Syntax (Atom (..), Sexp (..), Sym, pattern SexpAtom)
import Prettyprinter (Doc, defaultLayoutOptions, layoutSmart, pretty)
import Prettyprinter.Render.Text (renderStrict)

class ToSexp a where
  toSexp :: a -> Sexp

instance ToSexp Sexp where
  toSexp = id

instance (Functor f, ToSexp (f Sexp)) => ToSexp (Fix f) where
  toSexp = toSexp . fmap toSexp . unFix

instance (ToSexp s) => ToSexp (Anno k s) where
  toSexp = toSexp . annoVal

instance (Functor f, ToSexp (f Sexp)) => ToSexp (Memo f k) where
  toSexp = toSexp . fmap toSexp . memoVal

instance ToSexp Atom where
  toSexp = SexpAtom

instance ToSexp Sym where
  toSexp = SexpAtom . AtomSym

instance ToSexp Integer where
  toSexp = SexpAtom . AtomInt

instance ToSexp Int where
  toSexp = toSexp . fromIntegral @Int @Integer

instance ToSexp Scientific where
  toSexp = SexpAtom . AtomSci

instance ToSexp Text where
  toSexp = SexpAtom . AtomStr

instance ToSexp String where
  toSexp = toSexp . T.pack

instance ToSexp Char where
  toSexp = SexpAtom . AtomChar

toSexpDoc :: (ToSexp a) => a -> Doc ann
toSexpDoc = pretty . toSexp

toSexpText :: (ToSexp a) => a -> Text
toSexpText = renderStrict . layoutSmart defaultLayoutOptions . toSexpDoc
