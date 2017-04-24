{-# LANGUAGE DeriveGeneric #-}

module QuickForm.Many where

import Control.DeepSeq (NFData)
import Data.Monoid ((<>))
import Data.Aeson
import GHC.Generics (Generic)

-- | Pair of unrelated forms @a@ and @b@
data a :&: b = a :&: b
  deriving (Eq, Show, Generic)

instance (ToJSON a, ToJSON b) => ToJSON (a :&: b)
instance (FromJSON a, FromJSON b) => FromJSON (a :&: b)
instance (NFData a, NFData b) => NFData (a :&: b)
infixr 9 :&:

instance (Monoid a, Monoid b) => Monoid (a :&: b) where
  mempty = mempty :&: mempty
  mappend (a :&: b) (a' :&: b') = (a <> a') :&: (b <> b')
