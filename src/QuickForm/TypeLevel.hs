{-# LANGUAGE DeriveGeneric, ExistentialQuantification, KindSignatures
           , TypeFamilies, UndecidableInstances #-}

{-# LANGUAGE GADTs, PolyKinds, TypeInType #-}

module QuickForm.TypeLevel where

import Data.Aeson
import GHC.Generics (Generic)
import GHC.TypeLits (Symbol)
import Data.Kind

-- | QuickForm combinators. These are all used as lifted types by DataKinds.
data QuickForm where
  -- | Convert the sub form to a type
  Unvalidated :: Type -> QuickForm -> QuickForm
  -- | Validate the sub form to a failure type or a success type
  Validated :: Type -> Type -> QuickForm -> QuickForm
  -- | 'Field' wraps a base 'FieldType' with a 'Symbol' representing the
  -- HTML "name" property of a concrete field element
  Field :: Symbol -> FieldType -> QuickForm
  -- | Combine two 'QuickForm's to make a larger form
  (:+:) :: QuickForm -> QuickForm -> QuickForm

-- | Convert the sub form @b@ :: 'QuickForm' to type @a@.
-- This convenience type means you don't have to put ticks before using the
-- lifted constructor. It has kind 'QuickForm'.
type Unvalidated a b = 'Unvalidated a b
-- | Validate the sub form @b@ :: 'QuickForm' to type @a@ if successful, otherwise
-- to type @e@.
-- This convenience type means you don't have to put ticks before using the
-- lifted constructor. It has kind 'QuickForm'.
type Validated e a b = 'Validated e a b
-- | Represents a concrete HTML field element with name @n@ and type @t@ ::
-- 'FieldType'.
-- This convenience type means you don't have to put ticks before using the
-- lifted constructor. It has kind 'QuickForm'.
type Field n t = 'Field n t
-- | Combine @a@ :: 'QuickForm' with @b@ :: 'QuickForm'. This can be chained to
-- as many fields / sub forms as you require.
-- This convenience type means you don't have to put ticks before using the
-- lifted constructor. It has kind 'QuickForm'.
type (:+:) a b = a ':+: b
infixr 9 :+:

-- | Field types available
data FieldType where
  -- | Simple input field
  TextField :: FieldType
  -- | Hidden input field
  HiddenField :: FieldType
  -- | Enum field (dropdown or radio). Takes a type giving the data source.
  EnumField :: Type -> FieldType

-- | This convenience type means you don't have to put ticks before using the
-- lifted constructor. It has kind 'FieldType'.
type TextField = 'TextField
-- | This convenience type means you don't have to put ticks before using the
-- lifted constructor. It has kind 'FieldType'.
type HiddenField = 'HiddenField
-- | This convenience type means you don't have to put ticks before using the
-- lifted constructor. It has kind 'FieldType'.
type EnumField a = 'EnumField a

-- | Reading enum fields into their given type can fail (realistically only if a
-- user was to edit the HTML or send a custom request).
data EnumError = EnumReadFailed
  deriving (Eq, Show, Ord, Generic)
instance ToJSON EnumError
instance FromJSON EnumError

-- | Denotes a side of a combinator such as (a :+: b)
data WhichSide = First | Second | Both | Neither

-- | Finds which side the target form is on
type family FindSub (form :: QuickForm) (sub :: QuickForm) :: WhichSide where
  FindSub (a :+: b) c = HasSub a c `ChooseSide` HasSub b c
  FindSub (Validated _ _ b) c = 'False `ChooseSide` HasSub b c
  FindSub (Unvalidated _ b) c = 'False `ChooseSide` HasSub b c

-- | Determine if a form 'sub' is in 'form'
type family HasSub (form :: QuickForm) (sub :: QuickForm) :: Bool where
  HasSub a a = 'True
  HasSub (a :+: b) c = HasSub a c `Or` HasSub b c
  HasSub (Validated _ _ b) c = HasSub b c
  HasSub (Unvalidated _ b) c = HasSub b c
  HasSub a b = 'False

-- | Converts pair of 'Bool' to 'WhichSide'
type family ChooseSide (left :: Bool) (right :: Bool) :: WhichSide where
  ChooseSide 'True 'True = 'Both
  ChooseSide 'True 'False = 'First
  ChooseSide 'False 'True = 'Second
  ChooseSide 'False 'False = 'Neither

-- | Type level or function
type family Or (a :: Bool) (b :: Bool) :: Bool where
  Or 'True b = 'True
  Or a 'True = 'True
  Or a b = 'False

-- | Find which side of the type combinator we should explore
type family FindError (form :: QuickForm) :: WhichSide where
  FindError (Validated _ _ b) = 'True `ChooseSide` HasError b
  FindError (Unvalidated _ b) = 'False `ChooseSide` HasError b
  FindError (a :+: b) = HasError a `ChooseSide` HasError b
  FindError a = 'Neither

-- | Find if a given form has validated forms somewhere inside it
type family HasError (form :: QuickForm) :: Bool where
  HasError (Field _ ('EnumField _)) = 'True
  HasError (Field _ _) = 'False
  HasError (Validated _ _ _) = 'True
  HasError (Unvalidated _ b) = HasError b
  HasError (a :+: b) = HasError a `Or` HasError b

