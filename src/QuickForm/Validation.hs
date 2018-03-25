{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE PolyKinds #-}

module QuickForm.Validation where

import Data.Default

import QuickForm.Form
import QuickForm.TypeLevel

-- | Validation functions
data Validator (q :: QuickForm) where
  ValidatedVal :: (FormOutput q -> Either e a) -> Validator q -> Validator (Validated e a q)
  FieldVal :: Validator (Field n a)
  FieldsVal :: TList (Map Validator qs) -> Validator (Fields qs)

class ValidateForm q where
  validateForm :: Validator q -> Form q -> Result q

instance FormOutput (Field n a) ~ ToForm (Field n a) => ValidateForm (Field n a) where
  validateForm FieldVal (Form a) = Success a
  {-# INLINE validateForm #-}

instance (JoinFields qs, ValidateFields qs)
  => ValidateForm (Fields qs) where
  validateForm fs as = joinFields $ validateFields fs as
  {-# INLINE validateForm #-}

instance (Default (FormError q), ValidateForm q)
  => ValidateForm (Validated e a q) where
  validateForm (ValidatedVal f sub) (Form b)
    = runValidated f $ validateForm sub $ Form b
  {-# INLINE validateForm #-}

instance Default (Validator (Field n a)) where
  def = FieldVal
instance Default (TList (Map Validator qs))
  => Default (Validator (Fields qs)) where
    def = FieldsVal def

class ValidateFields qs where
  validateFields
    :: Validator (Fields qs) -> Form (Fields qs) -> TList (Map Result qs)

instance ValidateFields '[] where
  validateFields (FieldsVal Nil) (Form Nil) = Nil

instance
  ( ValidateForm q, ValidateFields qs
  ) => ValidateFields (q ': qs) where
    validateFields (FieldsVal (f :| fs)) (Form (a :| as)) =
      validateForm f (Form a) :| validateFields @qs (FieldsVal fs) (Form as)

-- | Result of running a validation
data Result q
  = Error (FormError q)
  | Success (FormOutput q)

deriving instance (Eq (FormError q), Eq (FormOutput q)) => Eq (Result q)
deriving instance (Show (FormError q), Show (FormOutput q)) => Show (Result q)

class JoinFields qs where
  joinFields :: TList (Map Result qs) -> Result (Fields qs)

instance JoinFields '[] where
  joinFields Nil = Success Nil

instance
  ( Default (TList (Map FormError qs))
  , Default (FormError q)
  , JoinFields qs
  ) => JoinFields (q ': qs) where
    joinFields (e :| es) = case (e, joinFields @qs es) of
      (Error e', Error (FieldsError es')) -> Error $ FieldsError $ e' :| es'
      (Error e', _) -> Error $ FieldsError $ e' :| def
      (_, Error (FieldsError es')) -> Error $ FieldsError $ def :| es'
      (Success a, Success as) -> Success $ a :| as

runValidated
  :: (Default (FormError q))
  => (FormOutput q -> Either e a) -> Result q -> Result (Validated e a q)
runValidated f q = case q of
  Error e -> Error $ ValidatedError Nothing e
  Success hs -> case f hs of
    Left e -> Error $ ValidatedError (Just e) def
    Right a -> Success a

