{-# LANGUAGE CPP, AllowAmbiguousTypes, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module QuickForm.BranchValidation where

import GHC.TypeLits (TypeError, ErrorMessage(..))
import Data.Kind

import QuickForm.Sub
import QuickForm.Many
import QuickForm.Form
import QuickForm.TypeLevel
import QuickForm.Validation

-- Validation by branch --------------------------------------------------------

validateBranch :: forall sub form. ValidatePartial form sub
               => Form 'Raw form -> Form 'Err form
validateBranch = validatePartial @form @sub

validatePartial :: forall form sub. ValidatePartial form sub
  => Form 'Raw form -> Form 'Err form
validatePartial = validatePartial' @(FindError form) @form @sub

type ValidatePartial form sub = ValidatePartial' (FindError form) form sub

class ValidatePartial' (errorLocation :: WhichSide) form sub where
  validatePartial' :: Form 'Raw form -> Form 'Err form

-- | Validated parents require full validation
instance {-# OVERLAPPABLE #-}
  ( ValidateAll form
  , EmptySetErrors (Reduce 'Err form)
  , form ~ (ValidatedForm e a :<: b)
  ) => ValidatePartial' err (ValidatedForm e a :<: b) sub where
    validatePartial'
      = either id (const $ Form emptySetErrors) . validateAll

-- | Terminal instance for validated forms
-- This instance is to resolve the overlap between (ValidatedForm e a :<: b) sub
-- and a a
instance {-# OVERLAPPABLE #-}
  ( ValidateAll form
  , EmptySetErrors (Reduce 'Err form)
  , form ~ (ValidatedForm e a :<: b)
  ) => ValidatePartial' err (ValidatedForm e a :<: b) (ValidatedForm e a :<: b) where
    validatePartial'
      = either id (const $ Form emptySetErrors) . validateAll

-- Unnamed unvalidated parents just validate the relavant sub sub
instance
  ( ValidatePartial b sub
  , HasError b ~ 'True
  ) => ValidatePartial' 'Second (UnvalidatedForm a :<: b) sub where
    validatePartial' = reform . validatePartial @b @sub . reform

-- | Found the sub, just validate it all
-- TODO check hasError is always correct
instance {-# OVERLAPPABLE #-}
  ( ValidateAll a
  , EmptySetErrors (ReduceErr a)
  , HasError a ~ 'True
  ) => ValidatePartial' err a a where
    validatePartial' = either id (const $ Form emptySetErrors) . validateAll @a

-- | Validate single branch of pair
instance ValidatePartialEither (a :&: b) sub
  => ValidatePartial' err (a :&: b) sub where
    validatePartial' = validatePartialEither @(a :&: b) @sub

validatePartialEither :: forall form sub. ValidatePartialEither form sub
  => Form 'Raw form -> Form 'Err form
validatePartialEither
  = validatePartialEither' @(FindSub form sub) @(FindError form) @form @sub

type ValidatePartialEither form sub
  = ValidatePartialEither' (FindSub form sub) (FindError form) form sub

class ValidatePartialEither' (fieldLocation :: WhichSide)
                             (errorLocation :: WhichSide) form sub where
  validatePartialEither' :: Form 'Raw form -> Form 'Err form

-- Validate left branch when right branch doesn't have errors
instance
  ( ValidatePartial a sub
  , HasError a ~ 'True
  , HasError b ~ 'False
  ) => ValidatePartialEither' 'First 'First (a :&: b) sub where
  validatePartialEither' (Form (a :&: _))
    = reform $ validatePartial @a @sub (Form a)

-- Validate left branch when right branch only has errors
instance
  ( ValidateAll a
  , HasError a ~ 'False
  , HasError b ~ 'True
  , Monoid (Reduce 'Err b)
  ) => ValidatePartialEither' 'First 'Second (a :&: b) sub where
  validatePartialEither' _ = Form mempty

-- Validate right branch when left branch only has errors
instance
  ( ValidateAll b
  , HasError a ~ 'True
  , HasError b ~ 'False
  , Monoid (Reduce 'Err a)
  ) => ValidatePartialEither' 'Second 'First (a :&: b) sub where
  validatePartialEither' _ = Form mempty

-- Validate right branch when left branch doesn't have errors
instance
  ( ValidatePartial b sub
  , HasError a ~ 'False
  , HasError b ~ 'True
  ) => ValidatePartialEither' 'Second 'Second (a :&: b) sub where
  validatePartialEither' (Form (_ :&: b))
    = reform $ validatePartial @b @sub (Form b)

-- Validate left branch when both branches have errors
instance
  ( ValidatePartial a sub
  , Monoid (Reduce 'Err b)
  , HasError a ~ 'True
  , HasError b ~ 'True
  ) => ValidatePartialEither' 'First 'Both (a :&: b) sub where
  validatePartialEither' (Form (a :&: _))
    = Form $ unForm (validatePartial @a @sub (Form a)) :&: mempty

-- Validate right branch when both branches have errors
instance
  ( ValidatePartial b sub
  , Monoid (Reduce 'Err a)
  , HasError a ~ 'True
  , HasError b ~ 'True
  ) => ValidatePartialEither' 'Second 'Both (a :&: b) sub where
  validatePartialEither' (Form (_ :&: b))
    = Form $ mempty :&: unForm (validatePartial @b @sub (Form b))

-- Custom type errors ----------------------------------------------------------

#ifndef DEV

-- | Catch all to show nicer errors
instance {-# OVERLAPPABLE #-} ValidatePartialError form sub
  => ValidatePartial' err form sub where
    validatePartial' = error "unreachable"

-- | Catch all to show nicer errors
instance {-# OVERLAPPABLE #-} ValidatePartialError form sub
  => ValidatePartialEither' next err form sub where
    validatePartialEither' = error "unreachable"

#endif

-- | Wrapper for 'ValidatePartialError''
type ValidatePartialError form sub
  = ValidatePartialError' form sub (HasSub form sub) (HasError form)

-- | Nice error messages for misusing partial validation, the right hand sides
-- should always be some 'TypeError'.
type family ValidatePartialError' form sub (exists :: Bool) (hasError :: Bool)
    :: Constraint where

  ValidatePartialError' form sub 'False e = TypeError
    ('Text "Attempted partial validation of sub"
    :$$: 'Text "  ‘" :<>: 'ShowType sub :<>: 'Text "’"
    :$$: 'Text "But the sub does not exist in the form"
    :$$: 'Text "  ‘" :<>: 'ShowType form :<>: 'Text "’")

  ValidatePartialError' form sub 'True 'False = TypeError
    ('Text "Attempted partial validation of sub"
    :$$: 'Text "  ‘" :<>: 'ShowType sub :<>: 'Text "’"
    :$$: 'Text "But the form it exists in is always valid"
    :$$: 'Text "  ‘" :<>: 'ShowType form :<>: 'Text "’")

  ValidatePartialError' form sub 'True 'True = TypeError
    ('Text "This should not happen: please report this bug!"
    :$$: 'Text "Attempted partial validation of sub"
    :$$: 'Text "  ‘" :<>: 'ShowType sub :<>: 'Text "’"
    :$$: 'Text "It exists in the form"
    :$$: 'Text "  ‘" :<>: 'ShowType form :<>: 'Text "’")

