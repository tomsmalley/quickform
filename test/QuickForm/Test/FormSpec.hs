{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module QuickForm.Test.FormSpec where

import Data.Aeson hiding (Result(..))
import Data.Default
import qualified Data.List.NonEmpty as NE
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S

import Test.Hspec
import Test.QuickCheck hiding (Result(..))
import Test.QuickCheck.Instances ()

import QuickForm
import QuickForm.Example
import QuickForm.Test.Orphans ()

spec :: Spec
spec = describe "Form" $ do

  describe "Form" $ do

    describe "Validation" $ do
      it "passes correctly" $ validateForm signupValidator
        (Form $ "user" :| ("password1" :| "password1" :| Nil)
          :| ("first" :| "middle" :| "last" :| Nil) :| "99999"
          :| ("" :| AnimalType_Other :| Nil) :| (Just Colour_Red :| Just Colour_Blue :| Nil) :| Nil)
        `shouldBe`
        Success (Username "user" :| Password "password1"
          :| Name "first" "middle" "last" :| Phone "99999"
          :| Animal "" AnimalType_Other :| (Colour_Red, Colour_Blue) :| Nil)

      it "fails correctly" $ do
        let rawForm = Form $ "user"
              :| ("pass1" :| "pass2" :| Nil)
              :| ("first" :| "middle" :| "last" :| Nil)
              :| "phone"
              :| ("" :| AnimalType_Other :| Nil)
              :| (Nothing :| Just Colour_Red :| Nil)
              :| Nil
            errForm = FieldsError $ def
              :| ValidatedError (Just $ PasswordError_TooShort NE.:| [PasswordError_DontMatch]) def
              :| def
              :| ValidatedError (Just PhoneError_NotANumber) def
              :| ValidatedError Nothing def
              :| ValidatedError (Just $ S.singleton ColourError_FavouriteMissing) def
              :| Nil
        validateForm signupValidator rawForm `shouldBe` Error errForm

    describe "JSON encoding" $ do
      it "roundtrips" $ property $ \(raw :: Form Form_SignUp) ->
        decode (encode raw) `shouldBe` Just raw
      it "is formatted correctly" $ property $ \u p1 p2 f m l p c1 c2 an at -> do
        let rawForm = Form @Form_SignUp $
              u :| (p1 :| p2 :| Nil) :| (f :| m :| l :| Nil) :| p
              :| (an :| at :| Nil) :| (c1 :| c2 :| Nil) :| Nil
        toJSON rawForm == Object (HM.fromList
          [ ("new-username", toJSON u)
          , ("password", toJSON p1)
          , ("password-repeat", toJSON p2)
          , ("name-first", toJSON f)
          , ("name-middle", toJSON m)
          , ("name-last", toJSON l)
          , ("phone", toJSON p)
          , ("animal-name", toJSON an)
          , ("animal-type", toJSON at)
          , ("colour-favourite", toJSON c1)
          , ("colour-least-favourite", toJSON c2)
          ])

  describe "FormErr" $ do
    it "roundtrips" $ property $ \(err :: FormError Form_SignUp) -> do
      decode (encode err) `shouldBe` Just err

