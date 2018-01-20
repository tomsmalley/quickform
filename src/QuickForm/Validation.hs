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

module QuickForm.Validation where

import Data.Default
import Data.Bifunctor (bimap)
import Data.Proxy
import Data.Set (Set)
import Data.Text (Text)

import QuickForm.Form
import QuickForm.TypeLevel

-- | Validation functions
data FormVal (q :: QuickForm) where
  ValidatedVal
    :: (FormHs q -> Either e a) -> FormVal q
    -> FormVal (Validated e a q)
  UnvalidatedVal
    :: (FormHs q -> a) -> FormVal q
    -> FormVal (Unvalidated a q)
  FieldVal :: FormVal (Field n a)
  FormVal :: TList (Map FormVal q) -> FormVal (SubForm q)


class Wf q => ValidateForm q where
  validateForm :: FormVal q -> FormRaw q -> ValResult q

instance ValidateForm (Field n Text) where
  validateForm FieldVal (FieldRaw raw) = Hs $ FieldHs $ raw
  {-# INLINE validateForm #-}

instance (Wf (SubForm qs), JoinSubForm qs, ValidateSubForm qs)
  => ValidateForm (SubForm qs) where
  validateForm fs as = joinSubForm $ validateSubForm fs as
  {-# INLINE validateForm #-}

instance ValidateForm q => ValidateForm (Unvalidated a q) where
  validateForm (UnvalidatedVal f sub) (UnvalidatedRaw b)
    = runUnvalidated f $ validateForm sub b
  {-# INLINE validateForm #-}

instance (Default e, Default (FormErr q), ValidateForm q)
  => ValidateForm (Validated e a q) where
  validateForm (ValidatedVal f sub) (ValidatedRaw b)
    = runValidated f $ validateForm sub b
  {-# INLINE validateForm #-}

class ValidateSubForm qs where
  validateSubForm
    :: FormVal (SubForm qs) -> FormRaw (SubForm qs) -> TList (Map ValResult qs)

instance ValidateSubForm '[] where
  validateSubForm (FormVal Nil) (FormRaw Nil) = Nil

instance
  ( ValidateForm q, ValidateSubForm qs
  ) => ValidateSubForm (q ': qs) where
    validateSubForm (FormVal (f :| fs)) (FormRaw (a :| as)) =
      validateForm f a :| validateSubForm @qs (FormVal fs) (FormRaw as)

-- | Result of running a validation
data ValResult q
  = Err (FormErr q)
  | Hs (FormHs q)

deriving instance (Eq (FormErr q), Eq (FormHs q)) => Eq (ValResult q)
deriving instance (Show (FormErr q), Show (FormHs q)) => Show (ValResult q)

valResult :: (FormErr q -> a) -> (FormHs q -> a) -> ValResult q -> a
valResult f _ (Err e) = f e
valResult _ f (Hs v) = f v

valErr :: ValResult q -> Maybe (FormErr q)
valErr (Err e) = Just e
valErr _ = Nothing

valHs :: ValResult q -> Maybe (FormHs q)
valHs (Hs v) = Just v
valHs _ = Nothing

class JoinSubForm qs where
  joinSubForm :: TList (Map ValResult qs) -> ValResult (SubForm qs)

instance JoinSubForm '[] where
  joinSubForm Nil = Hs $ FormHs Nil

instance
  ( Default (TList (Map FormErr qs))
  , Default (FormErr q)
  , JoinSubForm qs
  ) => JoinSubForm (q ': qs) where
    joinSubForm (e :| es) = case (e, joinSubForm @qs es) of
      (Err e, Err (FormErr es)) -> Err $ FormErr $ e :| es
      (Err e, _) -> Err $ FormErr $ e :| def
      (_, Err (FormErr es)) -> Err $ FormErr $ def :| es
      (Hs a, Hs (FormHs as)) -> Hs $ FormHs $ a :| as


runUnvalidated :: (FormHs q -> a) -> ValResult q -> ValResult (Unvalidated a q)
runUnvalidated f = \case
  Err e -> Err $ UnvalidatedErr e
  Hs hs -> Hs $ UnvalidatedHs $ f hs

runValidated
  :: (Default (FormErr q), Default e)
  => (FormHs q -> Either e a) -> ValResult q -> ValResult (Validated e a q)
runValidated f q = case q of
  Err e -> Err $ ValidatedErr def e
  Hs hs -> case f hs of
    Left e -> Err $ ValidatedErr e def
    Right a -> Hs $ ValidatedHs a


