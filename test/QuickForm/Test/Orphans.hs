{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module QuickForm.Test.Orphans where

import Test.QuickCheck.Arbitrary.Generic

import QuickForm

import QuickForm.Example

instance Arbitrary AnimalType where arbitrary = genericArbitrary
instance Arbitrary Colour where arbitrary = genericArbitrary
instance Arbitrary ColourError where arbitrary = genericArbitrary
instance Arbitrary NameError where arbitrary = genericArbitrary
instance Arbitrary PasswordError where arbitrary = genericArbitrary
instance Arbitrary PhoneError where arbitrary = genericArbitrary
instance Arbitrary UsernameError where arbitrary = genericArbitrary

instance Arbitrary (Form q) => Arbitrary (Form (Validated e a q)) where
  arbitrary = Form . unForm <$> arbitrary @(Form q)
instance Arbitrary a => Arbitrary (Form (Field n a)) where
  arbitrary = Form <$> arbitrary
instance Arbitrary (Form (Fields '[])) where
  arbitrary = pure $ Form Nil
instance (Arbitrary (Form a), Arbitrary (Form (Fields as)))
  => Arbitrary (Form (Fields (a ': as))) where
    arbitrary = (\(Form a) (Form as) -> Form $ a :| as)
      <$> arbitrary @(Form a) <*> arbitrary @(Form (Fields as))

instance (Arbitrary e, Arbitrary (FormError q)) => Arbitrary (FormError (Validated e a q)) where
  arbitrary = ValidatedError <$> arbitrary <*> arbitrary
instance Arbitrary (FormError (Field n a)) where
  arbitrary = pure FieldError
instance Arbitrary (FormError (Fields '[])) where
  arbitrary = pure $ FieldsError Nil
instance (Arbitrary (FormError a), Arbitrary (FormError (Fields as)))
  => Arbitrary (FormError (Fields (a ': as))) where
    arbitrary = (\a (FieldsError as) -> FieldsError $ a :| as)
      <$> arbitrary @(FormError a) <*> arbitrary @(FormError (Fields as))

