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

module QuickForm.Validation where

import Data.Proxy
import Data.Set (Set)

import QuickForm.Form
import QuickForm.TypeLevel


data ValFunc
type instance Args ValFunc (Validated e a sub)
  = (Form Hs sub -> Either (Set e) a)
type instance Args ValFunc (Unvalidated a sub)
  = (Form Hs sub -> a)
type instance Args ValFunc (Field n InputField) = Proxy n -- FIXME why is the name here and not elsewhere
type instance Args ValFunc (f1 :+: f2) = ()

runValidator
  :: Form ValFunc f -> Form Raw f
  -> Either (Form Err f) (Form Hs f)

runValidator (InputField _) (InputField (Proxy, raw))
  = Right $ InputField $ fromTouched "" raw

runValidator (Pair () f g) (Pair () a b)
  = runPair (runValidator f a) (runValidator g b)

runValidator (Unvalidated f sub) (Unvalidated () b)
  = runUnvalidated f $ runValidator sub b

runValidator (Validated f sub) (Validated () b)
  = runValidated f $ runValidator sub b
{-# INLINE runValidator #-}

--runValidator (EnumField f) (Form raw)
--  = case f $ fromTouched mempty raw of
--    Nothing -> Left $ Form $ Touched [EnumReadFailed]
--    Just a -> Right $ Form a


runPair
  :: (EmptySetErrors (Form Err f), EmptySetErrors (Form Err g))
  => Either (Form Err f) (Form Hs f)
  -> Either (Form Err g) (Form Hs g)
  -> Either (Form Err (f :+: g)) (Form Hs (f :+: g))
runPair (Left i1) (Left i2) = Left $ Pair () i1 i2
runPair (Left i1) _ = Left $ Pair () i1 emptySetErrors
runPair _ (Left i2) = Left $ Pair () emptySetErrors i2
runPair (Right j1) (Right j2) = Right $ Pair () j1 j2

runValidated
  :: EmptySetErrors (Form Err sub)
  => (Form Hs sub -> Either (Set e) a)
  -> Either (Form Err sub) (Form Hs sub)
  -> (Either (Form Err (Validated e a sub)) (Form Hs (Validated e a sub)))
runValidated f = \case
  Left e -> Left $ Validated mempty e
  Right hs -> case f hs of
    Left e -> Left $ Validated (Touched e) emptySetErrors
    Right a -> Right $ Validated a hs

runUnvalidated
  :: (Form Hs sub -> a)
  -> Either (Form Err sub) (Form Hs sub)
  -> Either (Form Err (Unvalidated a sub)) (Form Hs (Unvalidated a sub))
runUnvalidated f = \case
  Left e -> Left $ Unvalidated () e
  Right hs -> Right $ Unvalidated (f hs) hs

