{-# LANGUAGE CPP, AllowAmbiguousTypes, TypeFamilies, RankNTypes
           , TypeInType, UndecidableInstances #-}
{-# OPTIONS_HADDOCK hide #-}

module QuickForm.Lens where

import GHC.TypeLits (TypeError, ErrorMessage(..))
import Data.Kind

import QuickForm.Form
import QuickForm.TypeLevel

-- These are here to remove dependency on lens
type Lens' s a = Lens s s a a
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

-- | Generic lens to some sub form @sub@ in form @form@ of 'Reduced' type @r@.
-- The form is returned unpacked (i.e. not in 'Form') for convenience (no need
-- to use 'unForm' with every lens). Use with TypeApplications as such:
--
-- > myForm ^. subform @SubFieldType
subform :: forall s f r. (FormLens s f r)
      => Lens' (Form r f) (Reduce r s)
subform = subform' @s @f @r @(ChooseLens s f r)
{-# INLINE subform #-}

-- Choosing a lens implementation  ---------------------------------------------

-- | Use this promoted ADT in the instance head instead of having overlapping
-- instances, we can use a type family to choose which instance to use.
data LensType (s :: QuickForm) (f :: QuickForm) (r :: Reduced)
  = TerminalLens
  | SubLens QuickForm
  | SubLensErr
  | PairLens WhichSide
  | PairLensErr WhichSide
  | NoLens

-- | Type function chooses which instance to use for the lens. Essentially like
-- a pattern match but on type constructors instead of data constructors.
type family ChooseLens (s :: QuickForm) (f :: QuickForm) (r :: Reduced)
  :: LensType s f r where
    ChooseLens a a r = 'TerminalLens
    ChooseLens s f Err = ChooseErrLens (FindSub f s) (FindError f) s f
    ChooseLens s (a :+: b) r = ChoosePairLens (FindSub (a :+: b) s) s (a :+: b) r
    ChooseLens s (Unvalidated a b) Raw = 'SubLens b
    ChooseLens s (Validated e a b) Raw = 'SubLens b
    ChooseLens _ _ _ = 'NoLens

-- | Type function for pair lenses
type family ChoosePairLens (findSub :: WhichSide)
                           (s :: QuickForm) (f :: QuickForm) (r :: Reduced)
  :: LensType s f r where
    ChoosePairLens 'First s (a :+: b) r = 'PairLens 'First
    ChoosePairLens 'Second s (a :+: b) r = 'PairLens 'Second
    ChoosePairLens _ _ _ _ = 'NoLens

-- | Type function for error lenses
type family ChooseErrLens (findSub :: WhichSide) (findErr :: WhichSide)
                          (s :: QuickForm) (f :: QuickForm)
  :: LensType s f Err where
    ChooseErrLens 'Second 'Second s (Unvalidated a b) = 'SubLens b
    ChooseErrLens 'Second 'Both s (Validated e a b) = 'SubLensErr
    ChooseErrLens 'First 'Both s (a :+: b) = 'PairLens 'First
    ChooseErrLens 'First 'First s (a :+: b) = 'PairLensErr 'First
    ChooseErrLens 'Second 'Both s (a :+: b) = 'PairLens 'Second
    ChooseErrLens 'Second 'Second s (a :+: b) = 'PairLensErr 'Second
    ChooseErrLens _ _ _ _ = 'NoLens

-- Lens type class and instances -----------------------------------------------

-- | Convenience synonym
type FormLens s f r = FormLens' (ChooseLens s f r)

-- | Type class over the 'LensType' so we don't get lots of (essentially
-- identical implementation) overlapping / incoherent instances.
class FormLens' (lensType :: LensType s f r) where
  subform' :: Lens' (Form r f) (Reduce r s)

-- | Terminal lens
instance (Reduce r f ~ Reduce r s)
  => FormLens' ('TerminalLens :: LensType s f r) where
    subform' f (Form b) = Form <$> f b
    {-# INLINE subform' #-}

-- | General sub form lens
instance (FormLens s b r, Reduce r f ~ Reduce r b)
  => FormLens' ('SubLens b :: LensType s f r) where
    subform' f = fmap reform . subform @s @b @r f . reform
    {-# INLINE subform' #-}

-- | Validated error sub form lens
instance (FormLens s b Err, HasError b ~ 'True)
  => FormLens' ('SubLensErr :: LensType s (Validated e a b) Err) where
    subform' f (Form (a, b))
      = Form . (\b' -> (a, b')) . unForm <$> subform @s @b @Err f (Form b)
    {-# INLINE subform' #-}

-- | Generic pair lens, left side
instance (FormLens s a r, Reduce r (a :+: b) ~ (Reduce r a :*: Reduce r b))
  => FormLens' ('PairLens 'First :: LensType s (a :+: b) r) where
    subform' f (Form (a :+: b))
      = Form . (:+: b) . unForm <$> subform @s @a @r f (Form a)
    {-# INLINE subform' #-}

-- | Generic pair lens, right side
instance (FormLens s b r, Reduce r (a :+: b) ~ (Reduce r a :*: Reduce r b))
  => FormLens' ('PairLens 'Second :: LensType s (a :+: b) r) where
    subform' f (Form (a :+: b))
      = Form . (a :+:) . unForm <$> subform @s @b @r f (Form b)
    {-# INLINE subform' #-}

-- | Lens into the first error of a pair, where only the first has errors
instance (FormLens s a Err, HasError a ~ 'True, HasError b ~ 'False)
  => FormLens' ('PairLensErr 'First :: LensType s (a :+: b) Err) where
    subform' f = fmap reform . subform @s @a @Err f . reform
    {-# INLINE subform' #-}

-- | Lens into the second error of a pair, where only the second has errors
instance (FormLens s b Err, HasError a ~ 'False, HasError b ~ 'True)
  => FormLens' ('PairLensErr 'Second :: LensType s (a :+: b) Err) where
    subform' f = fmap reform . subform @s @b @Err f . reform
    {-# INLINE subform' #-}

-- Custom type errors ----------------------------------------------------------

#ifndef NO_FANCY_ERRORS

-- | Catch all to show nicer errors
instance LensError f s r
  => FormLens' ('NoLens :: LensType s f r) where
  subform' = error "unreachable"

#endif

-- | Wrapper for 'LensError''
type LensError (form :: QuickForm) (sub :: QuickForm) r
   = LensError' form sub (HasSub form sub) r

-- | Nice error messages for misusing lenses, the right hand sides should always
-- be some 'TypeError'.
type family LensError' (form :: QuickForm) (sub :: QuickForm)
                       (exists :: Bool) (r :: Reduced)
                         :: Constraint where

  LensError' form sub 'False r = TypeError
    ('Text "Attempted access of sub form"
    ':$$: 'Text "  ‘" ':<>: 'ShowType sub ':<>: 'Text "’"
    ':$$: 'Text "But it does not exist in the given form"
    ':$$: 'Text "  ‘" ':<>: 'ShowType form ':<>: 'Text "’")

  LensError' form sub 'True Err = TypeError
    ('Text "Attempted access of unvalidated sub form"
    ':$$: 'Text "  ‘" ':<>: 'ShowType sub ':<>: 'Text "’"
    ':$$: 'Text "It exists in the given form"
    ':$$: 'Text "  ‘" ':<>: 'ShowType form ':<>: 'Text "’"
    ':$$: 'Text "But not after the form is reduced to its error type"
    ':$$: 'Text "  ‘" ':<>: 'ShowType (Reduce Err form) ':<>: 'Text "’")

  LensError' form sub 'True Hs = TypeError
    ('Text "Attempted access of erased sub form"
    ':$$: 'Text "  ‘" ':<>: 'ShowType sub ':<>: 'Text "’"
    ':$$: 'Text "It exists in the given form"
    ':$$: 'Text "  ‘" ':<>: 'ShowType form ':<>: 'Text "’"
    ':$$: 'Text "But not after the form is reduced to its haskell type"
    ':$$: 'Text "  ‘" ':<>: 'ShowType (Reduce Hs form) ':<>: 'Text "’")

  LensError' form sub 'True Raw = TypeError
    ('Text "This should not happen: please report this bug!"
    ':$$: 'Text "Attempted access of sub form"
    ':$$: 'Text "  ‘" ':<>: 'ShowType sub ':<>: 'Text "’"
    ':$$: 'Text "It exists in the given form"
    ':$$: 'Text "  ‘" ':<>: 'ShowType form ':<>: 'Text "’"
    ':$$: 'Text "But not after the form is reduced to its raw type"
    ':$$: 'Text "  ‘" ':<>: 'ShowType (Reduce Raw form) ':<>: 'Text "’")

