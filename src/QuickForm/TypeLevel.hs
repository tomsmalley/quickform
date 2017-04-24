{-# LANGUAGE DeriveGeneric, ExistentialQuantification, KindSignatures
           , UndecidableInstances #-}

module QuickForm.TypeLevel where

import Data.Aeson
import GHC.Generics
import Data.Text (Text)
import Data.Set (Set)
import GHC.TypeLits (Symbol)
import Data.Kind

import QuickForm.Sub
import QuickForm.Many

-- | Lifted data kind, available reduced types: 'Raw' is the type which can
-- store the raw form values (e.g. Text from an <input> element), 'Hs' is the
-- validated haskell type, and 'Err' stores any errors that occur during
-- validation.
data Reduced = Raw | Hs | Err

-- | Denotes a side of a combinator such as (a :&: b)
data WhichSide = First | Second | Both | Neither

-- | Finds which side the target form is on
type family FindSub form sub :: WhichSide where
  FindSub (a :&: b) c = HasSub a c `ChooseSide` HasSub b c
  FindSub (a :<: b) c = HasSub a c `ChooseSide` HasSub b c

-- | Determine if a form 'sub' is in 'form'
type family HasSub form sub :: Bool where
  HasSub a a = 'True
  HasSub (a :&: b) c = HasSub a c `Or` HasSub b c
  HasSub (a :<: b) c = HasSub b c
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
type family FindError form :: WhichSide where
  FindError (a :<: b) = HasError a `ChooseSide` HasError b
  FindError (a :&: b) = HasError a `ChooseSide` HasError b
  FindError a = 'Neither

-- | Find if a given form has validated forms somewhere inside it
type family HasError (form :: Type) :: Bool where
  HasError (NamedForm n ('EnumForm a)) = 'True
  HasError (NamedForm n f) = 'False
  HasError (ValidatedForm e a) = 'True
  HasError (UnvalidatedForm a) = 'False
  HasError (ValidatedForm e a :<: _) = 'True
  HasError (UnvalidatedForm a :<: b) = HasError b
  HasError (a :&: b) = HasError a `Or` HasError b

-- | Use this instead of directly using type function
type ReduceErr a = ReduceErr' (FindError a) a

-- | Build the error type for a given form, only using the validated forms
type family ReduceErr' (which :: WhichSide) form :: Type where

  ReduceErr' 'First (a :<: b) = ReduceErr a
  ReduceErr' 'Second (a :<: b) = ReduceErr b
  ReduceErr' 'Both (a :<: b) = ReduceErr a :<: ReduceErr b

  ReduceErr' 'First (a :&: b) = ReduceErr a
  ReduceErr' 'Second (a :&: b) = ReduceErr b
  ReduceErr' 'Both (a :&: b) = ReduceErr a :&: ReduceErr b

  ReduceErr' w (ValidatedForm e _) = Maybe (Set e)
  ReduceErr' w (NamedForm _ ('EnumForm _)) = Maybe (Set EnumError)

-- | Get the shallowest output type of an element
type family OutputType form :: Type where
  OutputType (ValidatedForm _ o) = o
  OutputType (UnvalidatedForm o) = o
  OutputType (NamedForm _ 'TextForm) = Text
  OutputType (NamedForm _ 'HiddenForm) = Text
  OutputType (NamedForm _ ('EnumForm o)) = o

-- | Get the raw type of an element
type family RawType form :: Type where
  RawType (NamedForm _ 'TextForm) = Text
  RawType (NamedForm _ 'HiddenForm) = Text
  RawType (NamedForm _ ('EnumForm o)) = Text

type family Reduce (t :: Reduced) form :: Type where
  Reduce 'Err a = ReduceErr a
  Reduce 'Raw (a :<: b) = Reduce 'Raw b
  Reduce 'Hs (a :<: b) = Reduce 'Hs a
  Reduce t (a :&: b) = Reduce t a :&: Reduce t b

  Reduce 'Raw a = RawType a
  Reduce 'Hs a = OutputType a

-- | Chainable forms
data ValidatedForm (err :: Type) (output :: Type)
data UnvalidatedForm (output :: Type)

-- | Terminal form
data NamedForm (name :: Symbol) (form :: FormType)
data FormType
  = TextForm
  | HiddenForm
  | forall output. EnumForm (output :: Type)

data EnumError = EnumReadFailed
  deriving (Eq, Show, Ord, Generic)
instance ToJSON EnumError
instance FromJSON EnumError

