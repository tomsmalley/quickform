{-# LANGUAGE CPP, AllowAmbiguousTypes, TypeFamilies, RankNTypes
           , UndecidableInstances #-}
{-# OPTIONS_HADDOCK hide #-}

module QuickForm.Lenses where

import GHC.TypeLits (TypeError, ErrorMessage(..))
import Data.Kind

import QuickForm.Form
import QuickForm.Sub
import QuickForm.Many
import QuickForm.TypeLevel

-- These are here to remove dependency on lens
type Lens' s a = Lens s s a a
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

-- | Generic lens to some sub form @sub@ in form @form@ of 'Reduced' type @r@.
-- The form is returned unpacked (i.e. not in 'Form') for convenience (no need
-- to use 'unForm' with every lens). Use with TypeApplications as such:
--
-- > myForm ^. subform @SubFieldType
subform :: forall sub form r. FormLens form sub r
        => Lens' (Form r form) (Reduce r sub)
subform = subform' @form @sub
{-# INLINE subform #-}

-- Lenses over forms  ----------------------------------------------------------

-- | Lens type class
class FormLens form sub r where
  subform' :: Lens' (Form r form) (Reduce r sub)

-- | Lens into any pair form
instance FormLensEither (a :&: b) c r => FormLens (a :&: b) c r where
  subform' = formEither @(a :&: b) @c
  {-# INLINE subform' #-}

-- | Terminal lens, pair form, 'Hs' type
instance {-# OVERLAPPING #-} FormLens (a :&: b) (a :&: b) 'Hs where
  subform' f (Form b) = Form <$> f b
  {-# INLINE subform' #-}
-- | Terminal lens, sub form, 'Hs' type
instance {-# OVERLAPPING #-} FormLens (a :<: b) (a :<: b) 'Hs where
  subform' f (Form b) = Form <$> f b
  {-# INLINE subform' #-}
-- | Terminal lens, 'Hs' type
instance {-# OVERLAPPING #-} FormLens a a 'Hs where
  subform' f (Form b) = Form <$> f b
  {-# INLINE subform' #-}

-- | Terminal lens, sub form, 'Raw' type
instance {-# OVERLAPPING #-} FormLens (a :<: b) (a :<: b) 'Raw where
  subform' f (Form b) = Form <$> f b
  {-# INLINE subform' #-}
-- | Terminal lens, 'Raw' type
instance {-# OVERLAPPING #-} FormLens a a 'Raw where
  subform' f (Form b) = Form <$> f b
  {-# INLINE subform' #-}
-- | Lens into sub form of 'Raw' type
instance FormLens b c 'Raw => FormLens (a :<: b) c 'Raw where
  subform' f = fmap reform . subform' @b @c @'Raw f . reform
  {-# INLINE subform' #-}

-- | Terminal lens, pair form, 'Err' type
instance {-# OVERLAPPING #-} FormLens (a :&: b) (a :&: b) 'Err where
  subform' f (Form b) = Form <$> f b
  {-# INLINE subform' #-}
-- | Terminal lens, sub form, 'Err' type
instance {-# OVERLAPPING #-} FormLens (a :<: b) (a :<: b) 'Err where
  subform' f (Form b) = Form <$> f b
  {-# INLINE subform' #-}
-- | Terminal lens, 'Err' type
instance {-# OVERLAPPING #-} FormLens a a 'Err where
  subform' f (Form b) = Form <$> f b
  {-# INLINE subform' #-}
-- | Lens into sub form of 'Err' type
instance FormLensEither (a :<: b) c 'Err => FormLens (a :<: b) c 'Err where
  subform' = formEither @(a :<: b) @c
  {-# INLINE subform' #-}

-- Pair form lenses ------------------------------------------------------------

-- | Lenses into pair types (a :&: b), and (a :<: b) for 'Err' types.
-- This class uses overlap techniques described here:
-- <https://wiki.haskell.org/GHC/AdvancedOverlap>
-- It is identical to 'FormLens' except for two extra type parameters:
-- @tag@ is the side that the form we want is on, determined by 'ChooseLens'.
-- @err@ is the side that can contain errors, determined by 'ChooseErr'. This is
-- only useful for focusing on 'Err' forms and should be fully parametric in
-- other cases. We need a lot of instances to cope with the flexibility of 'Err'
-- forms.
class FormLensEither' (tag :: WhichSide) (err :: WhichSide) form sub r where
  formEither' :: Lens' (Form r form) (Reduce r sub)

type FormLensEither form sub r
  = FormLensEither' (FindSub form sub) (FindError form) form sub r

formEither :: forall form sub r. FormLensEither form sub r
            => Lens' (Form r form) (Reduce r sub)
formEither = formEither' @(FindSub form sub) @(FindError form) @form @sub @r
{-# INLINE formEither #-}

-- Raw value lenses, pair forms -----------------------------------------------

-- | Lens into the first value of a pair
instance FormLens a c 'Raw
  => FormLensEither' 'First err (a :&: b) c 'Raw where
    formEither' f (Form (a :&: b))
      = reform . fmap (:&: b) <$> subform' @a @c @'Raw f (Form a)
    {-# INLINE formEither' #-}

-- | Lens into the second value of a pair
instance FormLens b c 'Raw
  => FormLensEither' 'Second err (a :&: b) c 'Raw where
    formEither' f (Form (a :&: b))
      = reform . fmap (a :&:) <$> subform' @b @c @'Raw f (Form b)
    {-# INLINE formEither' #-}

-- Haskell type lenses, pair forms --------------------------------------------

-- | Lens into the first value of a pair
instance FormLens a c 'Hs
  => FormLensEither' 'First err (a :&: b) c 'Hs where
    formEither' f (Form (a :&: b))
      = reform . fmap (:&: b) <$> subform' @a @c @'Hs f (Form a)
    {-# INLINE formEither' #-}

-- | Lens into the second value of a pair
instance FormLens b c 'Hs
  => FormLensEither' 'Second err (a :&: b) c 'Hs where
    formEither' f (Form (a :&: b))
      = reform . fmap (a :&:) <$> subform' @b @c @'Hs f (Form b)
    {-# INLINE formEither' #-}

-- Error lenses, pair forms ---------------------------------------------------

-- | Lens into the first error of a pair, where both have errors
instance (FormLens a c 'Err, HasError a ~ 'True, HasError b ~ 'True)
  => FormLensEither' 'First 'Both (a :&: b) c 'Err where
    formEither' f (Form (a :&: b))
      = reform . fmap (:&: b) <$> subform' @a @c @'Err f (Form a)
    {-# INLINE formEither' #-}

-- | Lens into the second error of a pair, where both have errors
instance (FormLens b c 'Err, HasError a ~ 'True, HasError b ~ 'True)
  => FormLensEither' 'Second 'Both (a :&: b) c 'Err where
    formEither' f (Form (a :&: b))
      = reform . fmap (a :&:) <$> subform' @b @c @'Err f (Form b)
    {-# INLINE formEither' #-}

-- | Lens into the first error of a pair, where only the first has errors
instance (FormLens a c 'Err, HasError a ~ 'True, HasError b ~ 'False)
  => FormLensEither' 'First 'First (a :&: b) c 'Err where
    formEither' f
      = fmap reform . subform' @a @c @'Err f . reform
    {-# INLINE formEither' #-}

-- | Lens into the second error of a pair, where only the second has errors
instance (FormLens b c 'Err, HasError a ~ 'False, HasError b ~ 'True)
  => FormLensEither' 'Second 'Second (a :&: b) c 'Err where
    formEither' f
      = fmap reform . subform' @b @c @'Err f . reform
    {-# INLINE formEither' #-}

-- Error lenses, sub forms ----------------------------------------------------

-- | Lens into the top error of a form, where the sub form also has errors
instance (FormLens a c 'Err, HasError a ~ 'True, HasError b ~ 'True)
  => FormLensEither' 'First 'Both (a :<: b) c 'Err where
    formEither' f (Form (a :<: b))
      = reform . fmap (:<: b) <$> subform' @a @c @'Err f (Form a)
    {-# INLINE formEither' #-}

-- | Lens into the sub error of a form, where the top form also has errors
instance (FormLens b c 'Err, HasError a ~ 'True, HasError b ~ 'True)
  => FormLensEither' 'Second 'Both (a :<: b) c 'Err where
    formEither' f (Form (a :<: b))
      = reform . fmap (a :<:) <$> subform' @b @c @'Err f (Form b)
    {-# INLINE formEither' #-}

-- | Lens into the top error of a form, where the sub form doesn't have errors
instance (FormLens a c 'Err, HasError a ~ 'True, HasError b ~ 'False)
  => FormLensEither' 'First 'First (a :<: b) c 'Err where
    formEither' f
      = fmap reform . subform' @a @c @'Err f . reform
    {-# INLINE formEither' #-}

-- | Lens into the sub error of a form, where the top form doesn't have errors
instance (FormLens b c 'Err, HasError a ~ 'False, HasError b ~ 'True)
  => FormLensEither' 'Second 'Second (a :<: b) c 'Err where
    formEither' f
      = fmap reform . subform' @b @c @'Err f . reform
    {-# INLINE formEither' #-}

-- Custom type errors ----------------------------------------------------------

#ifndef DEV

-- | Catch all to show nicer errors
instance {-# OVERLAPPABLE #-} LensError form sub r
  => FormLensEither' l e form sub r where
    formEither' = error "unreachable"

-- | Catch all to show nicer errors
instance {-# OVERLAPPABLE #-} LensError form sub r
  => FormLens form sub r where
    subform' = error "unreachable"

#endif

-- | Wrapper for 'LensError''
type LensError form sub r = LensError' form sub (HasSub form sub) r

-- | Nice error messages for misusing lenses, the right hand sides should always
-- be some 'TypeError'.
type family LensError' form sub (exists :: Bool) (r :: Reduced)
  :: Constraint where

  LensError' form sub 'False r = TypeError
    ('Text "Attempted access of sub form"
    ':$$: 'Text "  ‘" ':<>: 'ShowType sub ':<>: 'Text "’"
    ':$$: 'Text "But it does not exist in the given form"
    ':$$: 'Text "  ‘" ':<>: 'ShowType form ':<>: 'Text "’")

  LensError' form sub 'True 'Err = TypeError
    ('Text "Attempted access of unvalidated sub form"
    ':$$: 'Text "  ‘" ':<>: 'ShowType sub ':<>: 'Text "’"
    ':$$: 'Text "It exists in the given form"
    ':$$: 'Text "  ‘" ':<>: 'ShowType form ':<>: 'Text "’"
    ':$$: 'Text "But not after the form is reduced to its error type"
    ':$$: 'Text "  ‘" ':<>: 'ShowType (ReduceErr form) ':<>: 'Text "’")

  LensError' form sub 'True 'Hs = TypeError
    ('Text "Attempted access of erased sub form"
    ':$$: 'Text "  ‘" ':<>: 'ShowType sub ':<>: 'Text "’"
    ':$$: 'Text "It exists in the given form"
    ':$$: 'Text "  ‘" ':<>: 'ShowType form ':<>: 'Text "’"
    ':$$: 'Text "But not after the form is reduced to its haskell type"
    ':$$: 'Text "  ‘" ':<>: 'ShowType (Reduce 'Hs form) ':<>: 'Text "’")

  LensError' form sub 'True 'Raw = TypeError
    ('Text "This should not happen: please report this bug!"
    ':$$: 'Text "Attempted access of sub form"
    ':$$: 'Text "  ‘" ':<>: 'ShowType sub ':<>: 'Text "’"
    ':$$: 'Text "It exists in the given form"
    ':$$: 'Text "  ‘" ':<>: 'ShowType form ':<>: 'Text "’"
    ':$$: 'Text "But not after the form is reduced to its raw type"
    ':$$: 'Text "  ‘" ':<>: 'ShowType (Reduce 'Raw form) ':<>: 'Text "’")

