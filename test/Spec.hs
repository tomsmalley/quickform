{-# LANGUAGE DataKinds, OverloadedStrings, TypeApplications,
   TypeOperators, FlexibleInstances #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}

import Control.Lens
import qualified Data.Set as S
import Data.Text
import Test.Hspec
import Test.QuickCheck
import Test.ShouldNotTypecheck (shouldNotTypecheck)

import GHC.TypeLits

import QuickForm
import QuickForm.TypeLevel


type TestField = Named "test" 'TextField

testFieldRaw :: Form 'Raw TestField
testFieldRaw = Form "testfield"

testFieldHs :: Form 'Hs TestField
testFieldHs = Form "testfield"

newtype Wrap = Wrap Text deriving Eq
type UField = Unvalidated Wrap TestField
instance Validation UField where
  validate (Form t) = Wrap t

ufieldRaw :: Form 'Raw UField
ufieldRaw = Form "ufield"

ufieldHs :: Form 'Hs UField
ufieldHs = Form $ Wrap "ufield"

data NTE = NTE deriving Eq
type VField = Validated NTE Wrap TestField
instance Validation VField where
  validate (Form "pass") = Valid $ Wrap "pass"
  validate (Form _) = Invalid $ S.singleton NTE

vfieldRawPass :: Form 'Raw VField
vfieldRawPass = Form "pass"

vfieldRawFail :: Form 'Raw VField
vfieldRawFail = Form "fail"

vfieldErr :: Form 'Err VField
vfieldErr = Form $ Just $ S.singleton NTE

vfieldErrEmpty :: Form 'Err VField
vfieldErrEmpty = Form $ Just S.empty

vfieldHs :: Form 'Hs VField
vfieldHs = Form $ Wrap "pass"

type TestField2 = Named "test2" 'TextField
type BasePair = TestField :+: TestField2

basePairRaw :: Form 'Raw BasePair
basePairRaw = Form $ "f1" :&: "f2"

basePairHs :: Form 'Hs BasePair
basePairHs = Form $ "f1" :&: "f2"

main :: IO ()
main = hspec $ do

  describe "Construction" $ do
    it "named text error field fails" $ do
      shouldNotTypecheck nte
    it "unvalidated form error field fails" $ do
      shouldNotTypecheck unte
    it "unvalidated pair error form fails" $ do
      shouldNotTypecheck ntpaire

  describe "Lenses" $ do

    describe "Raw unvalidated fields" $ do
      let form :: Form Raw (Unvalidated Int (Named "t" TextField))
          form = Form "1"
      it "view should work for total field" $ do
        form ^. subform @(Unvalidated Int (Named "t" TextField)) `shouldBe` "1"
      it "set should work for total field" $ do
        (form & subform @(Unvalidated Int (Named "t" TextField)) .~ "2")
          `shouldBe` Form "2"
      it "view should work for sub field" $ do
        form ^. subform @(Named "t" TextField) `shouldBe` "1"
      it "set should work for sub field" $ do
        (form & subform @(Named "t" TextField) .~ "2") `shouldBe` Form "2"

    describe "Raw validated fields" $ do
      let f :: Form Raw (Validated Bool Int (Named "t" TextField))
          f = Form "1"
      it "view" $ do
        f ^. subform @(Named "t" TextField) `shouldBe` "1"
      it "set" $ do
        (f & subform @(Named "t" TextField) .~ "2") `shouldBe` Form "2"
      it "view total" $ do
        f ^. subform @(Validated Bool Int (Named "t" TextField)) `shouldBe` "1"
      it "set total" $ do
        (f & subform @(Validated Bool Int (Named "t" TextField)) .~ "2")
          `shouldBe` Form "2"

    describe "Hs unvalidated fields" $ do
      let form :: Form Hs (Unvalidated Int (Named "t" TextField))
          form = Form 1
      it "should not typecheck for sub field access" $ do
        shouldNotTypecheck $ form ^. subform @(Named "t" TextField)
      it "view should work for total field" $ do
        form ^. subform @(Unvalidated Int (Named "t" TextField))
          `shouldBe` 1
      it "set should work for total field" $ do
        (form & subform @(Unvalidated Int (Named "t" TextField)) .~ 2)
          `shouldBe` Form 2

    describe "Err unvalidated fields" $ do
      let form :: Form Err (Unvalidated Int (Named "t" (EnumField Char)))
          form = Form err
          err = Just $ S.singleton EnumReadFailed
      it "should not compile unvalidated top field access" $ do
        shouldNotTypecheck $ form ^. subform @(Named "t" TextField) . topfield
      it "view should work for validated sub field" $ do
        form ^. subform @(Named "t" (EnumField Char)) `shouldBe` err
      it "set should work for validated sub field" $ do
        (form & subform @(Named "t" (EnumField Char)) .~ Nothing)
          `shouldBe` Form Nothing
      it "view should work for total field" $ do
        form ^. subform @(Unvalidated Int (Named "t" (EnumField Char)))
          `shouldBe` err
      it "set should work for total field" $ do
        (form & subform @(Unvalidated Int (Named "t" (EnumField Char))) .~ Nothing)
          `shouldBe` Form Nothing

    describe "Err validated fields" $ do
      let unvalSub :: Form Err (Validated Bool Int (Named "t" TextField))
          unvalSub = Form err
          err = Just $ S.singleton False
          err' = Just $ S.singleton True
          enumErr = Just $ S.singleton EnumReadFailed
          valSub :: Form Err (Validated Bool Int (Named "t" (EnumField Char)))
          valSub = Form $ err :<: enumErr
          valSub' :: Form Err (Validated Bool Int (Named "t" (EnumField Char)))
          valSub' = Form $ err :<: Nothing
      it "should not compile unvalidated subfield access" $ do
        shouldNotTypecheck $ unvalSub ^. subform @(Named "t" TextField)
      it "view should work for validated sub field" $ do
        valSub ^. subform @(Named "t" (EnumField Char)) `shouldBe` enumErr
      it "set should work for validated sub field" $ do
        (valSub & subform @(Named "t" (EnumField Char)) .~ Nothing)
          `shouldBe` valSub'
      it "view should work for total field" $ do
        valSub ^. subform @(Validated Bool Int (Named "t" (EnumField Char)))
          `shouldBe` err :<: enumErr
      it "view should work for top field" $ do
        valSub ^. subform @(Validated Bool Int (Named "t" (EnumField Char)))
                . topfield
          `shouldBe` err
      it "set should work for total field" $ do
        (valSub & subform @(Validated Bool Int (Named "t" (EnumField Char)))
                .~ (err' :<: Nothing))
                `shouldBe` Form (err' :<: Nothing)
      it "set should work for top field" $ do
        (valSub & subform @(Validated Bool Int (Named "t" (EnumField Char)))
                . topfield .~ err')
                `shouldBe` Form (err' :<: enumErr)

    describe "Raw pairs" $ do
      let form :: Form Raw (Named "1" TextField :+: Named "2" TextField)
          form = Form $ "1" :&: "2"
      it "view should work on the total field" $ do
        form ^. subform @(Named "1" TextField :+: Named "2" TextField)
          `shouldBe` "1" :&: "2"
      it "set should work on the total field" $ do
        (form & subform @(Named "1" TextField :+: Named "2" TextField)
          .~ "3" :&: "4")
          `shouldBe` Form ("3" :&: "4")
      it "view should work on the first field" $ do
        form ^. subform @(Named "1" TextField) `shouldBe` "1"
      it "set should work on the first field" $ do
        (form & subform @(Named "1" TextField) .~ "A")
          `shouldBe` Form ("A" :&: "2")
      it "view should work on the second field" $ do
        form ^. subform @(Named "2" TextField) `shouldBe` "2"
      it "set should work on the second field" $ do
        (form & subform @(Named "2" TextField) .~ "A")
          `shouldBe` Form ("1" :&: "A")

    describe "Hs pairs" $ do
      let form :: Form Hs (Named "1" (EnumField Bool) :+: Named "2" TextField)
          form = Form $ True :&: "2"
      it "view should work on the total field" $ do
        form ^. subform @(Named "1" (EnumField Bool) :+: Named "2" TextField)
          `shouldBe` True :&: "2"
      it "set should work on the total field" $ do
        (form & subform @(Named "1" (EnumField Bool) :+: Named "2" TextField)
              .~ False :&: "4")
          `shouldBe` Form (False :&: "4")
      it "view should work on the first field" $ do
        form ^. subform @(Named "1" (EnumField Bool)) `shouldBe` True
      it "set should work on the first field" $ do
        (form & subform @(Named "1" (EnumField Bool)) .~ False)
          `shouldBe` Form (False :&: "2")
      it "view should work on the second field " $ do
        form ^. subform @(Named "2" TextField) `shouldBe` "2"
      it "set should work on the second field" $ do
        (form & subform @(Named "2" TextField) .~ "A")
          `shouldBe` Form (True :&: "A")

    describe "Err pairs, both errors" $ do
      let form :: Form Err (Named "1" (EnumField Bool)
                           :+: Validated Int Text (Named "2" TextField))
          form = Form $ enumErr :&: intErr
          enumErr = Just $ S.singleton EnumReadFailed
          intErr = Just (S.singleton 1)
      it "view should work on the total field" $ do
        form ^. subform @(Named "1" (EnumField Bool)
                      :+: Validated Int Text (Named "2" TextField))
          `shouldBe` enumErr :&: intErr
      it "set should work on the total field" $ do
        (form & subform @(Named "1" (EnumField Bool)
                      :+: Validated Int Text (Named "2" TextField))
              .~ Nothing :&: Just S.empty)
          `shouldBe` Form (Nothing :&: Just S.empty)
      it "view should work on the first field" $ do
        form ^. subform @(Named "1" (EnumField Bool)) `shouldBe` enumErr
      it "set should work on the first field" $ do
        (form & subform @(Named "1" (EnumField Bool)) .~ Nothing)
          `shouldBe` Form (Nothing :&: intErr)
      it "view should work on the second field" $ do
        form ^. subform @(Validated Int Text (Named "2" TextField))
          `shouldBe` intErr
      it "set should work on the second field" $ do
        (form & subform @(Validated Int Text (Named "2" TextField)) .~ Nothing)
          `shouldBe` Form (enumErr :&: Nothing)

    describe "Err pairs, first errors" $ do
      let form :: Form Err (Named "1" (EnumField Bool) :+: Named "2" TextField)
          form = Form enumErr
          enumErr = Just $ S.singleton EnumReadFailed
      it "should not compile on second field access" $ do
        shouldNotTypecheck $ form ^. subform @(Named "2" TextField)
      it "view should work on the first field" $ do
        form ^. subform @(Named "1" (EnumField Bool)) `shouldBe` enumErr
      it "set should work on the first field" $ do
        (form & subform @(Named "1" (EnumField Bool)) .~ Nothing)
          `shouldBe` Form Nothing

    describe "Err pairs, second errors" $ do
      let form :: Form Err (Named "1" TextField :+: Named "2" (EnumField Bool))
          form = Form enumErr
          enumErr = Just $ S.singleton EnumReadFailed
      it "should not compile on first field access" $ do
        shouldNotTypecheck $ form ^. subform @(Named "1" TextField)
      it "view should work on the second field" $ do
        form ^. subform @(Named "2" (EnumField Bool)) `shouldBe` enumErr
      it "set should work on the second field" $ do
        (form & subform @(Named "2" (EnumField Bool)) .~ Nothing)
          `shouldBe` Form Nothing

  describe "Validation" $ do

    describe "Single text field form" $ do
      it "validates correctly" $ do
        validateAll testFieldRaw `shouldBe` testFieldHs

    describe "Single unvalidated parent form" $ do
      it "validates correctly" $ do
        validateAll ufieldRaw `shouldBe` ufieldHs

    describe "Single validated parent form" $ do
      it "passes validation correctly" $ do
        validateAll vfieldRawPass `shouldBe` Right vfieldHs
      it "fails validation correctly" $ do
        validateAll vfieldRawFail `shouldBe` Left vfieldErr

    describe "Base pair" $ do
      it "validates correctly" $ do
        validateAll basePairRaw `shouldBe` basePairHs

  describe "Branch validation" $ do

    describe "Single text field form" $ do
      it "fails branch validation in the typechecker" $ do
        shouldNotTypecheck $ validateBranch @TestField testFieldRaw

    describe "Single unvalidated parent form" $ do
      it "fails branch validation in the typechecker" $ do
        shouldNotTypecheck $ validateBranch @UField ufieldRaw

    describe "Base pair" $ do
      it "fails branch validation in the typechecker" $ do
        shouldNotTypecheck $ validateBranch @BasePair basePairRaw

    describe "Single validated parent form" $ do
      it "passes top level branch validation correctly" $ do
        validateBranch @VField vfieldRawPass `shouldBe` vfieldErrEmpty
      it "fails top level branch validation correctly" $ do
        validateBranch @VField vfieldRawFail `shouldBe` vfieldErr
      it "passes sub form branch validation correctly" $ do
        validateBranch @TestField vfieldRawPass `shouldBe` vfieldErrEmpty
      it "fails sub form branch validation correctly" $ do
        validateBranch @TestField vfieldRawFail `shouldBe` vfieldErr

  describe "Joining errors" $ do
    it "not implemented" False









