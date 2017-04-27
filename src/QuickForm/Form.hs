{-# LANGUAGE DeriveFunctor, DeriveGeneric, KindSignatures #-}

module QuickForm.Form where

import Control.DeepSeq (NFData)
import Data.Monoid ((<>))
import Data.Aeson
import GHC.Generics (Generic)

import QuickForm.TypeLevel

-- | Concrete form values
type Form (r :: Reduced) (form :: QuickForm) = Form' r form (Reduce r form)

-- | Like 'Tagged' but with extra tag @r@ of kind 'Reduced'.
newtype Form' (r :: Reduced) (form :: QuickForm) a = Form { unForm :: a }
  deriving (Eq, Functor, Generic)

instance ToJSON a => ToJSON (Form' r form a)
instance FromJSON a => FromJSON (Form' r form a)
instance NFData a => NFData (Form' r form a)

instance Monoid a => Monoid (Form' r form a) where
  mempty = Form mempty
  Form a `mappend` Form b = Form $ a <> b

instance Show a => Show (Form' r form a) where
  showsPrec n (Form a) = showParen (n > 10)
    $ showString "Form "
    . showsPrec 11 a

-- | Repack the form to update the phantom types
reform :: Form' r form a -> Form' t' f' a
reform = Form . unForm
