module Mello.Print
  ( ToSexp (..)
  , toSexpDoc
  , toSexpText
  )
where

import Data.Text (Text)
import Mello.Syntax (Sexp)
import Prettyprinter (Doc, defaultLayoutOptions, layoutSmart, pretty)
import Prettyprinter.Render.Text (renderStrict)

class ToSexp a where
  toSexp :: a -> Sexp

toSexpDoc :: (ToSexp a) => a -> Doc ann
toSexpDoc = pretty . toSexp

toSexpText :: (ToSexp a) => a -> Text
toSexpText = renderStrict . layoutSmart defaultLayoutOptions . toSexpDoc
