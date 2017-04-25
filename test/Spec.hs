{-# LANGUAGE DataKinds, OverloadedStrings, TypeApplications #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}

import Control.Lens
import qualified Data.Set as S
import Data.Text
import Test.Hspec
import Test.QuickCheck
import Test.ShouldNotTypecheck (shouldNotTypecheck)

import QuickForm


type TestField = NamedField "test" 'TextField

testFieldRaw :: Form 'Raw TestField
testFieldRaw = Form "testfield"

testFieldHs :: Form 'Hs TestField
testFieldHs = Form "testfield"

newtype NT = NT Text deriving Eq
type UField = UnvalidatedForm NT :<: TestField
instance Validation UField where
  validate (Form t) = NT t

ufieldRaw :: Form 'Raw UField
ufieldRaw = Form "ufield"

ufieldHs :: Form 'Hs UField
ufieldHs = Form $ NT "ufield"

data NTE = NTE deriving Eq
type VField = ValidatedForm NTE NT :<: TestField
instance Validation VField where
  validate (Form "pass") = Valid $ NT "pass"
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
vfieldHs = Form $ NT "pass"

type TestField2 = NamedField "test2" 'TextField
type BasePair = TestField :&: TestField2

basePairRaw :: Form 'Raw BasePair
basePairRaw = Form $ "f1" :&: "f2"

basePairHs :: Form 'Hs BasePair
basePairHs = Form $ "f1" :&: "f2"


main :: IO ()
main = hspec $ do

  describe "Lenses" $ do

    describe "Raw pair terminal lens" $ do
      it "works" $ do
        basePair ^. subform @BasePair `shouldBe` unForm basePairRaw


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









