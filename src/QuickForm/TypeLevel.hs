{-# LANGUAGE DeriveGeneric, ExistentialQuantification, KindSignatures
           , TypeFamilies, UndecidableInstances #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module QuickForm.TypeLevel
  (
    QuickForm
  , Unvalidated
  , Validated
  , Field
  , SubForm
  , Map
  , TMap
  , HList (..)

  ) where

import Control.DeepSeq
import Data.Text (Text)
import Data.Aeson
import GHC.Generics (Generic)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Data.Kind (Type)
import Data.Proxy
import Data.Default

import qualified Data.Text as T

symbolText :: KnownSymbol s => Proxy s -> Text
symbolText = T.pack . symbolVal

data HList (a :: [*]) where
  (:|) :: a -> HList as -> HList (a ': as)
  HNil :: HList '[]
infixr 8 :|

instance Default (HList '[]) where
  def = HNil
instance (Default a, Default (HList as)) => Default (HList (a ': as)) where
  def = def :| def

instance Show (HList '[]) where
  show HNil = "HNil"
instance (Show a, Show (HList as)) => Show (HList (a ': as)) where
  show (a :| as) = unwords [show a, ":|", show as]

-- | This is promoted to a kind, and forms the basis of the library. See
-- below for explanations of the constructors.
data QuickForm where
  Unvalidated :: a -> QuickForm -> QuickForm
  Validated :: e -> a -> QuickForm -> QuickForm
  Field :: Symbol -> a -> QuickForm
  SubForm :: [QuickForm] -> QuickForm

-- Old

type family ToRaw (f :: QuickForm) :: * where
  ToRaw (Validated e a f) = ToRaw f
  ToRaw (Unvalidated a f) = ToRaw f
  ToRaw (Field n a) = a
  ToRaw (SubForm (f ': fs)) = HList (MapToRaw (f ': fs))

type family MapToRaw (f :: [QuickForm]) :: [*] where
  MapToRaw '[] = '[]
  MapToRaw (f ': fs) = (ToRaw f ': MapToRaw fs)


--nameRaw = PureRaw $ FormRaw $ HCons (ValidRaw (InputRaw "fn")) (HCons (PureRaw (InputRaw "ln")) HNil)

--  | SubForm [QuickForm]

-- | Convert the sub form @b@ :: 'QuickForm' to type @a@.
type Unvalidated a (b :: QuickForm) = 'Unvalidated a b
-- | Validate the sub form @b@ :: 'QuickForm' to type @a@ if successful,
-- otherwise to type @e@.
type Validated e a b = 'Validated e a b
-- | Represents a concrete HTML field element with name @n@ and type @t@ ::
-- 'FieldType'.
type Field n t = 'Field n t
-- | Combine @a@ :: 'QuickForm' with @b@ :: 'QuickForm'. This can be chained to
-- as many fields / sub forms as you require.
type SubForm fs = 'SubForm fs

-- Type functions --------------------------------------------------------------

type family Map (f :: QuickForm -> *) (qs :: [QuickForm]) :: [*] where
  Map _ '[] = '[]
  Map f (q ': qs) = f q ': Map f qs

type family TMap (f :: * -> *) (qs :: [*]) :: [*] where
  TMap _ '[] = '[]
  TMap f (q ': qs) = f q ': TMap f qs

