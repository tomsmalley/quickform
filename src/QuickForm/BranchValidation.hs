{-# LANGUAGE CPP, AllowAmbiguousTypes, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_HADDOCK hide #-}

-- | This module encapsulates branch-wise validation of forms (i.e. only
-- validating the relevant fields).
module QuickForm.BranchValidation where

import GHC.TypeLits (TypeError, ErrorMessage(..))
import Data.Kind

import QuickForm.Form
import QuickForm.TypeLevel
import QuickForm.Validation

-- Validation by branch --------------------------------------------------------

-- | Validate a specific part of a form. Use a type application to specify
-- @sub@, and this function will only validate the relevant branch of the form.
-- Cases where fields are not validated are returned as `Nothing`, fields which
-- are validated and successful are returned as `Just` `mempty`.
validateBranch :: forall sub form. ValidateBranch sub form
               => Form Raw form -> Form Err form
validateBranch = validateBranch' @(FindSub form sub) @(FindError form) @sub

-- | Ensure we apply the correct type functions when used as a constraint
type ValidateBranch sub form
  = ValidateBranch' (FindSub form sub) (FindError form) sub form

-- | Partial validation class for "pair" types (anything where the shape changes
-- depending on where the errors are). FindSub and FindError are "pulled up" to
-- the instance head so that we can pattern match on them, this overlap
-- technique is described here: <https://wiki.haskell.org/GHC/AdvancedOverlap>
-- Don't use this class / function except to define instances, instead use the
-- unticked versions above.
class ValidateBranch' (findSub :: WhichSide) (findError :: WhichSide)
                      (sub :: QuickForm) (form :: QuickForm) where
  validateBranch' :: Form Raw form -> Form Err form

-- | Determine if all raw fields have been touched by the user
class AllTouched (form :: QuickForm) where
  allTouched :: Form Raw form -> Bool

instance (AllTouched a, AllTouched b) => AllTouched (a :+: b) where
  allTouched (Form (a :+: b)) = allTouched @a (Form a) && allTouched @b (Form b)
instance AllTouched b => AllTouched (Unvalidated a b) where
  allTouched = allTouched @b . reform
instance AllTouched b => AllTouched (Validated e a b) where
  allTouched = allTouched @b . reform
instance AllTouched (Field l n t) where
  allTouched (Form Untouched) = False
  allTouched (Form (Touched _)) = True

-- Instances -------------------------------------------------------------------

-- | Found the sub in the form, just validate it all
instance
  ( ValidateAll a
  , EmptySetErrors (Reduce Err a)
  , HasError a ~ 'True
  ) => ValidateBranch' findSub r a a where
    validateBranch' = either id (const $ Form emptySetErrors) . validateAll @a

-- | Validated parents with validated sub forms
instance {-# INCOHERENT #-}
  ( ValidateAll form
  , EmptySetErrors (Reduce Err form)
  , form ~ (Validated e a b)
  , ValidateBranch sub b
  , HasError b ~ 'True
  , AllTouched b
  ) => ValidateBranch' findSub 'Both sub (Validated e a b) where
    validateBranch' raw
      = case validateAll raw of
          Right _ -> Form emptySetErrors
          Left (Form e) ->
            if allTouched raw then Form e
            else Form $ (mempty, unForm $ validateBranch @sub @b $ reform raw)

-- | Validated parents with unvalidated sub forms
instance {-# INCOHERENT #-}
  ( ValidateAll form
  , EmptySetErrors (Reduce Err form)
  , Monoid (Reduce Err form)
  , form ~ (Validated e a b)
  , HasError b ~ 'False
  , AllTouched b
  ) => ValidateBranch' findSub 'First sub (Validated e a b) where
    validateBranch' raw
      = case validateAll raw of
        Right _ -> Form emptySetErrors
        Left (Form e) -> if allTouched raw then Form e else Form mempty

-- | Unnamed unvalidated parents just partially validate the relevant sub form
instance {-# INCOHERENT #-}
  ( ValidateBranch sub b
  , HasError b ~ 'True
  ) => ValidateBranch' findSub 'Second sub (Unvalidated a b) where
    validateBranch' = reform . validateBranch @sub @b . reform

-- Pair instances --------------------------------------------------------------

-- | Validate left branch when right branch doesn't have errors
instance
  ( ValidateBranch sub a
  , HasError a ~ 'True
  , HasError b ~ 'False
  ) => ValidateBranch' 'First 'First sub (a :+: b) where
  validateBranch' (Form (a :+: _))
    = reform $ validateBranch @sub @a (Form a)

-- | Validate right branch when left branch doesn't have errors
instance
  ( ValidateBranch sub b
  , HasError a ~ 'False
  , HasError b ~ 'True
  ) => ValidateBranch' 'Second 'Second sub (a :+: b) where
  validateBranch' (Form (_ :+: b))
    = reform $ validateBranch @sub @b (Form b)

-- | Validate left branch when right branch only has errors
instance
  ( ValidateAll a
  , HasError a ~ 'False
  , HasError b ~ 'True
  , Monoid (Reduce Err b)
  ) => ValidateBranch' 'First 'Second sub (a :+: b) where
  validateBranch' _ = Form mempty

-- | Validate right branch when left branch only has errors
instance
  ( ValidateAll b
  , HasError a ~ 'True
  , HasError b ~ 'False
  , Monoid (Reduce Err a)
  ) => ValidateBranch' 'Second 'First sub (a :+: b) where
  validateBranch' _ = Form mempty

-- | Validate left branch when both branches have errors
instance
  ( ValidateBranch sub a
  , Monoid (Reduce Err b)
  , HasError a ~ 'True
  , HasError b ~ 'True
  ) => ValidateBranch' 'First 'Both sub (a :+: b) where
  validateBranch' (Form (a :+: _))
    = Form $ unForm (validateBranch @sub @a (Form a)) :+: mempty

-- | Validate right branch when both branches have errors
instance
  ( ValidateBranch sub b
  , Monoid (Reduce Err a)
  , HasError a ~ 'True
  , HasError b ~ 'True
  ) => ValidateBranch' 'Second 'Both sub (a :+: b) where
  validateBranch' (Form (_ :+: b))
    = Form $ mempty :+: unForm (validateBranch @sub @b (Form b))

-- Custom type errors ----------------------------------------------------------

#ifndef NO_FANCY_ERRORS

-- | Catch all to show nicer errors
instance {-# OVERLAPS #-} ValidatePartialError sub form
  => ValidateBranch' next err sub form where
    validateBranch' = error "unreachable"

#endif

-- | Wrapper for 'ValidatePartialError''
type ValidatePartialError sub form
  = ValidatePartialError' sub form (HasSub form sub) (HasError form)

-- | Nice error messages for misusing partial validation, the right hand sides
-- should always be some 'TypeError'.
type family ValidatePartialError' (sub :: QuickForm) (form :: QuickForm)
                                  (exists :: Bool) (hasError :: Bool)
                                  :: Constraint where

  ValidatePartialError' sub form 'False e = TypeError
    ('Text "Attempted partial validation of sub"
    ':$$: 'Text "  ‘" ':<>: 'ShowType sub ':<>: 'Text "’"
    ':$$: 'Text "But the sub does not exist in the form"
    ':$$: 'Text "  ‘" ':<>: 'ShowType form ':<>: 'Text "’")

  ValidatePartialError' sub form 'True 'False = TypeError
    ('Text "Attempted partial validation of sub"
    ':$$: 'Text "  ‘" ':<>: 'ShowType sub ':<>: 'Text "’"
    ':$$: 'Text "But the form it exists in is always valid"
    ':$$: 'Text "  ‘" ':<>: 'ShowType form ':<>: 'Text "’")

  ValidatePartialError' sub form 'True 'True = TypeError
    ('Text "This should not happen: please report this bug!"
    ':$$: 'Text "Attempted partial validation of sub"
    ':$$: 'Text "  ‘" ':<>: 'ShowType sub ':<>: 'Text "’"
    ':$$: 'Text "It exists in the form"
    ':$$: 'Text "  ‘" ':<>: 'ShowType form ':<>: 'Text "’")

