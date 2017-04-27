{-# LANGUAGE DeriveGeneric, TypeFamilies, UndecidableInstances #-}

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
import QuickForm.Pair

-- | 'Form' reduces the given 'QuickForm' type @f@ to the 'Reduced' type @r@,
-- keeping both types as phantom types.
newtype Form (r :: Reduced) (f :: QuickForm) = Form { unForm :: Reduce r f }
  deriving (Generic)

-- | Repack the form to update the phantom types
reform :: (Reduce r f ~ Reduce r' f') => Form r f -> Form r' f'
reform = Form . unForm

-- Instances -------------------------------------------------------------------

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

-- Type functions --------------------------------------------------------------

-- | Lifted data kind, available reduced types: 'Raw' is the type which can
-- store the raw form values (e.g. Text from an <input> element), 'Hs' is the
-- validated haskell type, and 'Err' stores any errors that occur during
-- validation.
data Reduced = Raw | Hs | Err

-- | This convenience type means you don't have to put ticks before using the
-- lifted constructor. It has kind 'Reduced'.
type Raw = 'Raw
-- | This convenience type means you don't have to put ticks before using the
-- lifted constructor. It has kind 'Reduced'.
type Err = 'Err
-- | This convenience type means you don't have to put ticks before using the
-- lifted constructor. It has kind 'Reduced'.
type Hs = 'Hs

-- | Reduce the given 'QuickForm' @f@ to the 'Reduced' type @r@. Converts the
-- kind 'QuickForm' to the kind 'Type' ('*') so that term level structures can
-- be built.
type family Reduce (r :: Reduced) (f :: QuickForm) :: Type where
  Reduce 'Err a = ReduceError a
  Reduce 'Raw (Validated _ _ b) = Reduce 'Raw b
  Reduce 'Raw (Unvalidated _ b) = Reduce 'Raw b
  Reduce 'Hs (Validated _ a _) = a
  Reduce 'Hs (Unvalidated a _) = a
  Reduce r (a :+: b) = Reduce r a :*: Reduce r b
  Reduce 'Raw (Field _ f) = RawType f
  Reduce 'Hs (Field _ f) = OutputType f

-- | Get the raw type of an element
type family RawType (form :: FieldType) :: Type where
  RawType 'TextField = Text
  RawType 'HiddenField = Text
  RawType ('EnumField _) = Text

-- | Get the shallowest output type of an element
type family OutputType (form :: FieldType) :: Type where
  OutputType 'TextField = Text
  OutputType 'HiddenField = Text
  OutputType ('EnumField o) = o

-- | Ensure use of the correct type function
type ReduceError a = ReduceError' (FindError a) a

-- | Specialised 'Reduce' for 'Err', needs an extra type argument @w@ which
-- shows which "side" the next errors are on so that we can cut out irrelevant
-- parts.
type family ReduceError' (w :: WhichSide) (f :: QuickForm) :: Type where
  ReduceError' 'First (Validated e _ _) = Maybe (Set e)
  ReduceError' 'Both (Validated e _ b) = (Maybe (Set e), ReduceError b)
  ReduceError' 'Second (Unvalidated _ b) = ReduceError b
  ReduceError' 'First (a :+: b) = ReduceError a
  ReduceError' 'Second (a :+: b) = ReduceError b
  ReduceError' 'Both (a :+: b) = ReduceError a :*: ReduceError b
  ReduceError' w (Field _ ('EnumField _)) = Maybe (Set EnumError)

