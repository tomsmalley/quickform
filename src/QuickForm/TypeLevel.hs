{-# LANGUAGE DeriveGeneric, ExistentialQuantification, KindSignatures
           , TypeFamilies, UndecidableInstances #-}

{-# LANGUAGE GADTs, PolyKinds, TypeInType #-}

module QuickForm.TypeLevel where

import Data.Aeson
import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Set (Set)
import GHC.TypeLits (Symbol)
import Data.Kind

import QuickForm.Pair

-- | Lifted data kind, available reduced types: 'Raw' is the type which can
-- store the raw form values (e.g. Text from an <input> element), 'Hs' is the
-- validated haskell type, and 'Err' stores any errors that occur during
-- validation.
data Reduced = Raw | Hs | Err
type Raw = 'Raw
type Err = 'Err
type Hs = 'Hs

-- | Denotes a side of a combinator such as (a :*: b)
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
  HasError (Named _ ('EnumField _)) = 'True
  HasError (Named _ _) = 'False
  HasError (Validated _ _ _) = 'True
  HasError (Unvalidated _ b) = HasError b
  HasError (a :+: b) = HasError a `Or` HasError b

-- | Use this instead of directly using type function
type ReduceErr a = ReduceErr' (FindError a) a

-- | Build the error type for a given form, only using the validated forms
type family ReduceErr' (which :: WhichSide) (form :: QuickForm) :: Type where

  ReduceErr' 'First (Validated e _ _) = Maybe (Set e)
  ReduceErr' 'Both (Validated e _ b) = (Maybe (Set e), ReduceErr b)
  ReduceErr' 'Second (Unvalidated _ b) = ReduceErr b

  ReduceErr' 'First (a :+: b) = ReduceErr a
  ReduceErr' 'Second (a :+: b) = ReduceErr b
  ReduceErr' 'Both (a :+: b) = ReduceErr a :*: ReduceErr b

  ReduceErr' w (Named _ ('EnumField _)) = Maybe (Set EnumError)

-- | Get the shallowest output type of an element
type family OutputType (form :: FieldType) :: Type where
  OutputType 'TextField = Text
  OutputType 'HiddenField = Text
  OutputType ('EnumField o) = o

-- | Get the raw type of an element
type family RawType (form :: FieldType) :: Type where
  RawType 'TextField = Text
  RawType 'HiddenField = Text
  RawType ('EnumField _) = Text

type family Reduce (r :: Reduced) (form :: QuickForm) :: Type where
  Reduce 'Err a = ReduceErr a
  Reduce 'Raw (Validated _ _ b) = Reduce 'Raw b
  Reduce 'Raw (Unvalidated _ b) = Reduce 'Raw b
  Reduce 'Hs (Validated _ a _) = a
  Reduce 'Hs (Unvalidated a _) = a
  Reduce r (a :+: b) = Reduce r a :*: Reduce r b

  Reduce 'Raw (Named _ f) = RawType f
  Reduce 'Hs (Named _ f) = OutputType f

-- | Chainable forms

data EnumError = EnumReadFailed
  deriving (Eq, Show, Ord, Generic)
instance ToJSON EnumError
instance FromJSON EnumError

type TextField = 'TextField
type HiddenField = 'HiddenField
type EnumField a = 'EnumField a

data FieldType where
  TextField :: FieldType
  HiddenField :: FieldType
  EnumField :: Type -> FieldType

type Unvalidated = 'Unvalidated
type Validated = 'Validated
type Named = 'Named
type (:+:) a b = a ':+: b

data QuickForm where
  Unvalidated :: Type -> QuickForm -> QuickForm
  Validated :: Type -> Type -> QuickForm -> QuickForm
  Named :: Symbol -> FieldType -> QuickForm
  (:+:) :: QuickForm -> QuickForm -> QuickForm

infixr 9 :+:
