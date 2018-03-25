{-# LANGUAGE ExistentialQuantification, KindSignatures
           , TypeFamilies, UndecidableInstances #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE Rank2Types #-}

module QuickForm.TypeLevel where

import Data.Semigroup (Semigroup(..))
import GHC.TypeLits
import Data.Kind (Type, Constraint)
import Data.Default

append :: TList as -> TList bs -> TList (Append as bs)
append Nil ys = ys
append (x :| xs) ys = x :| append xs ys

-- | Heterogeneous list
data TList (a :: [Type]) where
  Nil :: TList '[]
  (:|) :: a -> TList as -> TList (a ': as)
infixr 8 :|

instance Show (TList '[]) where
  showsPrec _ Nil = showString "Nil"
instance (Show a, Show (TList as)) => Show (TList (a ': as)) where
  showsPrec d (x :| xs) = showParen (d > fixity)
    $ showsPrec (succ fixity) x
    . showString " :| "
    . showsPrec (succ fixity) xs
      where fixity = 8

instance Eq (TList '[]) where
  Nil == Nil = True
instance (Eq a, Eq (TList as)) => Eq (TList (a ': as)) where
  x :| xs == y :| ys = x == y && xs == ys

instance Ord (TList '[]) where
  Nil `compare` Nil = EQ
instance (Ord a, Ord (TList as)) => Ord (TList (a ': as)) where
  (x :| xs) `compare` (y :| ys) = case x `compare` y of
    EQ -> xs `compare` ys
    c -> c

instance Default (TList '[]) where
  def = Nil
instance (Default a, Default (TList as)) => Default (TList (a ': as)) where
  def = def :| def

instance Semigroup (TList '[]) where
  Nil <> Nil = Nil
instance (Semigroup a, Semigroup (TList as)) => Semigroup (TList (a ': as)) where
  (a :| as) <> (a' :| as') = (a <> a') :| (as <> as')

instance Monoid (TList '[]) where
  mempty = Nil
  mappend = (<>)
instance (Semigroup a, Semigroup (TList as), Monoid a, Monoid (TList as))
  => Monoid (TList (a ': as)) where
  mempty = mempty :| mempty
  mappend = (<>)


-- | This is promoted to a kind, and forms the basis of the library. See
-- below for explanations of the constructors.
data QuickForm where
  Validated :: e -> a -> QuickForm -> QuickForm
  Field :: Symbol -> a -> QuickForm
  Fields :: [QuickForm] -> QuickForm

-- | Validate the sub form @q@ :: 'QuickForm' to type @a@ if successful,
-- otherwise to type @e@.
type Validated e a q = 'Validated e a q
-- | Represents a concrete HTML field element with name @n@ and type @a@
type Field n a = 'Field n a
-- | Type level list of sub forms.
type Fields qs = 'Fields qs

-- Type functions --------------------------------------------------------------

-- | Type level (kind polymorphic) 'map'
type family Map (f :: k -> k') (qs :: [k]) :: [k'] where
  Map _ '[] = '[]
  Map f (q ': qs) = f q ': Map f qs

-- | Extract a list of field names from a 'QuickForm'
type family Names (q :: QuickForm) :: [Symbol] where
  Names (Validated e a q) = Names q
  Names (Field n a) = '[n]
  Names (Fields qs) = Concat (MapNames qs)

class Wf (q :: QuickForm)
instance UniqueConstraint (Names q) => Wf q

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

