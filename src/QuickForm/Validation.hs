{-# LANGUAGE DeriveGeneric, AllowAmbiguousTypes, TypeFamilies
           , UndecidableInstances #-}
{-# OPTIONS_HADDOCK hide #-}

module QuickForm.Validation where

import Control.DeepSeq (NFData)
import Text.Read (readMaybe)
import Data.Text (Text, unpack)
import GHC.Generics (Generic)
import Data.Set (Set)
import qualified Data.Set as S

import QuickForm.Sub
import QuickForm.Many
import QuickForm.Form
import QuickForm.TypeLevel

data Validate err hs
  = Invalid (Set err)
  | Valid hs
  deriving (Eq, Show, Generic)
instance Ord err => Monoid (Validate err hs) where
  mempty = Invalid mempty
  Invalid s `mappend` Invalid s' = Invalid $ s `mappend` s'
  Valid hs `mappend` _ = Valid hs
  _ `mappend` Valid hs = Valid hs
instance (NFData err, NFData hs) => NFData (Validate err hs)

-- | Like monoid's mempty, but produces Just mempty instead of Nothing
class EmptySetErrors a where
  emptySetErrors :: a
instance Monoid a => EmptySetErrors (Maybe a) where
  emptySetErrors = Just mempty
instance (EmptySetErrors a, EmptySetErrors b) => EmptySetErrors (a :<: b) where
  emptySetErrors = emptySetErrors :<: emptySetErrors
instance (EmptySetErrors a, EmptySetErrors b) => EmptySetErrors (a :&: b) where
  emptySetErrors = emptySetErrors :&: emptySetErrors

type family ValidationType form where
  ValidationType (UnvalidatedForm a :<: b) = Form 'Hs b -> a
  ValidationType (ValidatedForm e a :<: b) = Form 'Hs b -> Validate e a
  ValidationType (NamedField n ('EnumField a)) = Text -> Validate EnumError a

-- Any FormSet can be validated
class Validation a where
  validate :: ValidationType a

-- Validation ------------------------------------------------------------------

-- | For guiding GHC to the correct types
validateAll :: forall f. ValidateAll f
     => Form 'Raw f -> ValidateAllType (HasError f) f
validateAll = validateAll'

-- | For guiding GHC to the correct types
type ValidateAll f = ValidateAll' (FindError f) f

type family ValidateAllType (hasError :: Bool) form where
  ValidateAllType 'False f = Form 'Hs f
  ValidateAllType 'True f = Either (Form 'Err f) (Form 'Hs f)

-- | Need to pull errorLocation out into the class head so that we can match on
-- it for the different cases
class
  ( FindError f ~ errorLocation
  ) => ValidateAll' (errorLocation :: WhichSide) f where
    validateAll' :: Form 'Raw f -> ValidateAllType (HasError f) f

-- Base
instance ValidateAll' 'Neither (NamedField n 'TextField) where
  validateAll' = reform

instance ValidateAll' 'Neither (NamedField n 'HiddenField) where
  validateAll' = reform

-- | Base enum
instance Read a => ValidateAll' 'Neither (NamedField n ('EnumField a)) where
    validateAll' (Form t) = case readMaybe $ unpack t of
      Nothing -> Left $ Form $ Just $ S.singleton EnumReadFailed
      Just a  -> Right $ Form a

-- Unvalidated parent with sub validation
instance
  ( ValidateAll b, HasError b ~ 'True
  , Validation form
  , form ~ (UnvalidatedForm a :<: b)
  ) => ValidateAll' 'Second (UnvalidatedForm a :<: b) where
    validateAll' (Form b)
      = case validateAll @b (Form b) of
        Left (Form err) -> Left $ Form err
        Right form -> Right . Form $ validate @form form

-- Unvalidated parent without sub validation
instance
  ( ValidateAll b, HasError b ~ 'False
  , Validation form
  , form ~ (UnvalidatedForm a :<: b)
  ) => ValidateAll' 'Neither (UnvalidatedForm a :<: b) where
    validateAll'
      = Form . validate @form . validateAll @b . reform

-- Validated parent with sub validation
instance
  ( ValidateAll b, HasError b ~ 'True
  , Validation form
  , form ~ (ValidatedForm e a :<: b)
  , EmptySetErrors (ReduceErr b)
  , Ord e
  ) => ValidateAll' 'Both (ValidatedForm e a :<: b) where
    validateAll' (Form b)
      = case validateAll @b (Form b) of
        Left (Form err) -> Left $ Form $ mempty :<: err
        Right form -> case validate @form form of
          Invalid err -> Left $ Form $ Just err :<: emptySetErrors
          Valid v' -> Right $ Form v'

-- Validated parent without sub validation
instance
  ( ValidateAll b, HasError b ~ 'False
  , Validation form
  , form ~ (ValidatedForm e a :<: b)
  ) => ValidateAll' 'First (ValidatedForm e a :<: b) where
    validateAll' (Form b)
      = case validate @form $ validateAll @b (Form b) of
          Invalid err -> Left $ Form $ Just err
          Valid v' -> Right $ Form v'

-- Pair type where both sides have validation
instance
  ( ValidateAll a, HasError a ~ 'True
  , ValidateAll b, HasError b ~ 'True
  , EmptySetErrors (ReduceErr a)
  , EmptySetErrors (ReduceErr b)
  ) => ValidateAll' 'Both (a :&: b) where
    validateAll' (Form (a :&: b))
      = case (validateAll @a (Form a), validateAll @b (Form b)) of
             (Left a', Left b') -> Left . Form $ unForm a' :&: unForm b'
             (Left a', _) -> Left . Form $ unForm a' :&: emptySetErrors
             (_, Left b') -> Left . Form $ emptySetErrors :&: unForm b'
             (Right a', Right b') -> Right . Form $ unForm a' :&: unForm b'

-- Pair type where the first/left side has validation
instance
  ( ValidateAll a, HasError a ~ 'True
  , ValidateAll b, HasError b ~ 'False
  ) => ValidateAll' 'First (a :&: b) where
    validateAll' (Form (a :&: b))
      = case validateAll @a (Form a) of
          Left a' -> Left $ reform a'
          Right a' -> Right . Form $ unForm a' :&: unForm b'
        where b' = validateAll @b (Form b)

-- Pair type where the second/right side has validation
instance
  ( ValidateAll a, ValidateAll b
  , HasError a ~ 'False, HasError b ~ 'True
  ) => ValidateAll' 'Second (a :&: b) where
    validateAll' (Form (a :&: b))
      = case validateAll @b (Form b) of
          Left b' -> Left $ reform b'
          Right b' -> Right . Form $ unForm a' :&: unForm b'
        where a' = validateAll @a (Form a)

-- Pair type where neither side have validation
instance
  ( ValidateAll a, HasError a ~ 'False
  , ValidateAll b, HasError b ~ 'False
  ) => ValidateAll' 'Neither (a :&: b) where
    validateAll' (Form (a :&: b))
      = Form $ unForm a' :&: unForm b'
        where a' = validateAll @a (Form a)
              b' = validateAll @b (Form b)

