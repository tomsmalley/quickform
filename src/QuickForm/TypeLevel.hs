{-# LANGUAGE DeriveGeneric, ExistentialQuantification, KindSignatures
           , TypeFamilies, UndecidableInstances #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE Rank2Types #-}

module QuickForm.TypeLevel
  (
    QuickForm
  , Unvalidated
  , Validated
  , Field
  , SubForm
  , Map
  , Wf
  , Unique
  , Names
  , All
  , TList (..)
  , MapNames
  , Concat

  , symbolText

  , Elem

  ) where

import Control.DeepSeq
import Data.Text (Text)
import Data.Aeson
import GHC.Generics (Generic)
import GHC.TypeLits
import Data.Kind (Type, Constraint)
import Data.Proxy
import Data.Default

import qualified Data.Text as T

symbolText :: KnownSymbol s => Proxy s -> Text
symbolText = T.pack . symbolVal

--append :: TList as -> TList bs -> TList (Append as bs)
--append Nil ys = ys
--append (x :| xs) ys = x :| append xs ys

-- | HList as a data family. This is lifted from the package 'HList'. The data
-- family representation allows deriving instances, whereas the GADT
-- representation does not.
--
-- > data TList (a :: [Type]) where
-- >   (:|) :: a -> TList as -> TList (a ': as)
-- >   Nil :: TList '[]
data family TList (as :: [Type])
data instance TList '[] = Nil
data instance TList (a ': as) = a :| TList as
infixr 8 :|

deriving instance Generic (TList '[])
deriving instance (Generic a, Generic (TList as)) => Generic (TList (a ': as))
deriving instance Show (TList '[])
deriving instance (Show a, Show (TList as)) => Show (TList (a ': as))
deriving instance Eq (TList '[])
deriving instance (Eq a, Eq (TList as)) => Eq (TList (a ': as))
deriving instance Ord (TList '[])
deriving instance (Ord a, Ord (TList as)) => Ord (TList (a ': as))

instance Default (TList '[]) where
  def = Nil
instance (Default a, Default (TList as)) => Default (TList (a ': as)) where
  def = def :| def

-- | This is promoted to a kind, and forms the basis of the library. See
-- below for explanations of the constructors.
data QuickForm where
  Unvalidated :: a -> QuickForm -> QuickForm
  Validated :: e -> a -> QuickForm -> QuickForm
  Field :: Symbol -> a -> QuickForm
  SubForm :: [QuickForm] -> QuickForm

-- | Convert the sub form @q@ :: 'QuickForm' to type @a@.
type Unvalidated a q = 'Unvalidated a q
-- | Validate the sub form @q@ :: 'QuickForm' to type @a@ if successful,
-- otherwise to type @e@.
type Validated e a q = 'Validated e a q
-- | Represents a concrete HTML field element with name @n@ and type @a@
type Field n a = 'Field n a
-- | Type level list of sub forms.
type SubForm qs = 'SubForm qs

-- Type functions --------------------------------------------------------------

-- | Type level (kind polymorphic) 'map'
type family Map (f :: k -> k') (qs :: [k]) :: [k'] where
  Map _ '[] = '[]
  Map f (q ': qs) = f q ': Map f qs


-- | Extract a list of field names from a 'QuickForm'
type family Names (q :: QuickForm) :: [Symbol] where
  Names (Validated e a q) = Names q
  Names (Unvalidated a q) = Names q
  Names (Field n a) = '[n]
  Names (SubForm qs) = Concat (MapNames qs)

class UniqueConstraint (Names q) => Wf (q :: QuickForm)
instance Wf q => Wf (Validated e a q)
instance Wf q => Wf (Unvalidated a q)
instance (UniqueConstraint (Concat (MapNames qs)), All Wf qs) => Wf (SubForm qs)
instance Wf (Field n a)

-- | Ensure that all elements in the list satisfy the given constraint
type family All (f :: k -> Constraint) (as :: [k]) :: Constraint where
  All _ '[] = ()
  All f (a ': as) = (f a, All f as)

type family UniqueConstraint (as :: [k]) :: Constraint where
  UniqueConstraint as = IfThenElse (Unique as) (() :: Constraint)
    (TypeError ('Text "Duplicate field names in form:" ':$$: 'ShowType as
          ':$$: 'Text "All field names must be unique."))

-- | Type level if..then..else
type family IfThenElse (p :: Bool) (a :: k) (b :: k) :: k where
  IfThenElse 'True a _ = a
  IfThenElse 'False _ b = b

-- | Check that the given list contains no duplicates (i.e. is a set)
type family Unique (as :: [k]) :: Bool where
  Unique as = GoUnique '[] as

type family GoUnique (existing :: [k]) (list :: [k]) :: Bool where
  GoUnique _ '[] = 'True
  GoUnique prev (a ': as) = Not (Elem a prev) `And` GoUnique (a ': prev) as

-- | Type level '&&'
type family And (a :: Bool) (b :: Bool) :: Bool where
  And 'True 'True = 'True
  And _ _ = 'False

-- | Type level 'not'
type family Not (a :: Bool) :: Bool where
  Not 'True = 'False
  Not 'False = 'True

-- | Type level 'elem', check if the element exists in the list
type family Elem (a :: k) (as :: [k]) :: Bool where
  Elem _ '[] = 'False
  Elem a (a ': as) = 'True
  Elem a (_ ': as) = Elem a as

-- Since `Map Names` doesn't work
type family MapNames (q :: [QuickForm]) :: [[Symbol]] where
  MapNames '[] = '[]
  MapNames (q ': qs) = Names q ': MapNames qs

-- | Type level 'concat'
type family Concat (as :: [[k]]) :: [k] where
  Concat '[] = '[]
  Concat (l ': ls) = l `Append` Concat ls

-- | Type level '++'
type family Append (as :: [k]) (bs :: [k]) :: [k] where
  Append '[] bs = bs
  Append (a ': as) bs = a ': Append as bs

