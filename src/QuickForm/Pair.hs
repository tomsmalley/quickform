{-# LANGUAGE DeriveGeneric #-}

module QuickForm.Pair where

import Control.DeepSeq (NFData)
import Data.Monoid ((<>))
import Data.Aeson
import GHC.Generics (Generic)

-- | Pair of unrelated forms @a@ and @b@
data a :*: b = a :*: b
  deriving (Eq, Generic)

infixr 9 :*:

instance (ToJSON a, ToJSON b) => ToJSON (a :*: b)
instance (FromJSON a, FromJSON b) => FromJSON (a :*: b)
instance (NFData a, NFData b) => NFData (a :*: b)

instance (Show a, Show b) => Show (a :*: b) where
  showsPrec n (a :*: b) = showParen (n > 10)
    $ showsPrec 10 a
    . showString " :*: "
    . showsPrec 10 b

instance (Monoid a, Monoid b) => Monoid (a :*: b) where
  mempty = mempty :*: mempty
  mappend (a :*: b) (a' :*: b') = (a <> a') :*: (b <> b')
