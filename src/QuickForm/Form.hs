{-# LANGUAGE DeriveFunctor, DeriveGeneric, TypeFamilies
           , UndecidableInstances #-}

-- | Form wrapper
module QuickForm.Form where

import Control.DeepSeq (NFData)
import Data.Aeson
import Data.Kind
import Data.Set (Set)
import Data.Text (Text)
import Data.Monoid ((<>))
import GHC.Generics (Generic)

import QuickForm.TypeLevel

-- Form type -------------------------------------------------------------------

-- | 'Form' reduces the given 'QuickForm' type @f@ to the 'Reduced' type @r@.
-- This stores any actual form data. Both types @f@ and @r@ are retained as
-- phantom types for inference.
newtype Form (r :: Reduced) (f :: QuickForm) = Form { unForm :: Reduce r f }
  deriving (Generic)

-- | Repack the form to update the phantom types. Useful in cases where the
-- reduced representation is identical for two sets of phantom types.
reform :: (Reduce r f ~ Reduce r' f') => Form r f -> Form r' f'
reform = Form . unForm

instance Eq (Reduce r f) => Eq (Form r f) where
  Form r == Form r' = r == r'

instance ToJSON (Reduce r f) => ToJSON (Form r f) where
  toJSON (Form r) = toJSON r

instance FromJSON (Reduce r f) => FromJSON (Form r f) where
  parseJSON r = Form <$> parseJSON r

instance NFData (Reduce r f) => NFData (Form r f)

instance Monoid (Reduce r f) => Monoid (Form r f) where
  mempty = Form mempty
  Form a `mappend` Form b = Form $ a <> b

instance Show (Reduce r f) => Show (Form r f) where
  showsPrec n (Form a) = showParen (n > 10)
    $ showString "Form "
    . showsPrec 11 a

-- Pair type -------------------------------------------------------------------

-- | Pair of unrelated fields @a@ and @b@, both of kind 'Type'.
data a :*: b = a :+: b
  deriving (Eq, Generic)

infixr 9 :+:

instance (ToJSON a, ToJSON b) => ToJSON (a :*: b)
instance (FromJSON a, FromJSON b) => FromJSON (a :*: b)
instance (NFData a, NFData b) => NFData (a :*: b)

instance (Show a, Show b) => Show (a :*: b) where
  showsPrec n (a :+: b) = showParen (n > 10)
    $ showsPrec 10 a
    . showString " :+: "
    . showsPrec 10 b

instance (Monoid a, Monoid b) => Monoid (a :*: b) where
  mempty = mempty :+: mempty
  mappend (a :+: b) (a' :+: b') = (a <> a') :+: (b <> b')

-- Touched type ----------------------------------------------------------------

-- | Like 'Maybe' but with a different monoid instance
data Touched a = Untouched | Touched a
  deriving (Eq, Functor, Generic, Read, Show)

instance ToJSON a => ToJSON (Touched a)
instance FromJSON a => FromJSON (Touched a)
instance NFData a => NFData (Touched a)

instance Monoid (Touched a) where
  mempty = Untouched
  mappend _ (Touched a) = Touched a
  mappend a _ = a

fromTouched :: a -> Touched a -> a
fromTouched a Untouched = a
fromTouched _ (Touched a) = a

-- Type functions --------------------------------------------------------------

-- | Encodes how a 'QuickForm' is reduced by the type family 'Reduce'. Promoted
-- to a kind, see below for the constructors.
data Reduced = Raw | Hs | Err

-- | Stores the raw form values (e.g. Text from an <input> element)
type Raw = 'Raw
-- | Stores the validated haskell types
type Err = 'Err
-- | Stores errors from validation
type Hs = 'Hs

-- | Reduce the given 'QuickForm' @f@ to the 'Reduced' type @r@. Converts the
-- kind 'QuickForm' to the kind 'Type' ('*') so that term level structures can
-- be built.
type family Reduce (r :: Reduced) (f :: QuickForm) :: Type where
  Reduce Err a = ReduceError a
  Reduce Raw (Validated _ _ b) = Reduce Raw b
  Reduce Raw (Unvalidated _ b) = Reduce Raw b
  Reduce Hs (Validated _ a _) = a
  Reduce Hs (Unvalidated a _) = a
  Reduce r (a :+: b) = Reduce r a :*: Reduce r b
  Reduce Raw (Field _ f) = Touched (RawType f)
  Reduce Hs (Field _ f) = OutputType f

-- | Get the raw type of an element
type family RawType (form :: FieldType) :: Type where
  RawType InputField = Text
  RawType (EnumField _) = Text

-- | Get the shallowest output type of an element
type family OutputType (form :: FieldType) :: Type where
  OutputType InputField = Text
  OutputType (EnumField o) = o

-- | Ensure use of the correct type function
type ReduceError a = ReduceError' (FindError a) a

-- | Specialised 'Reduce' for 'Err', needs an extra type argument @w@ which
-- shows which "side" the next errors are on so that we can cut out irrelevant
-- parts.
type family ReduceError' (w :: WhichSide) (f :: QuickForm) :: Type where
  ReduceError' 'First (Validated e _ _) = Touched (Set e)
  ReduceError' 'Both (Validated e _ b) = (Touched (Set e), ReduceError b)
  ReduceError' 'Second (Unvalidated _ b) = ReduceError b
  ReduceError' 'First (a :+: b) = ReduceError a
  ReduceError' 'Second (a :+: b) = ReduceError b
  ReduceError' 'Both (a :+: b) = ReduceError a :*: ReduceError b
  ReduceError' w (Field _ (EnumField _)) = Touched (Set EnumError)

