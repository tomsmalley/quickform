{-# LANGUAGE DataKinds, OverloadedStrings, TypeApplications,
   TypeOperators, FlexibleInstances #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}

import Control.Lens
import qualified Data.Set as S
import Data.Monoid ((<>))
import Data.Text
import Test.Hspec
import Test.QuickCheck
import Test.ShouldNotTypecheck (shouldNotTypecheck)

import QuickForm

-- Main ------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
  construction
  formNewtype
  pair
  enumError
  validation
  branchValidation
  lenses

-- Shared test data ------------------------------------------------------------

justEnumErr :: Checked (S.Set EnumError)
justEnumErr = Checked $ S.singleton EnumReadFailed

type N1 = Field "T" (InputField TextInput)
type N2 = Field "H" (InputField HiddenInput)
type N3 = Field "E" (EnumField Int)

type V1 = Validated Bool Text (Field "e" (EnumField Int))
type V2 = Validated String Int V1
instance Validation V1 where
  validate (Form 1) = Right "1"
  validate (Form 2) = Right "2"
  validate _ = Left $ S.singleton False
instance Validation V2 where
  validate (Form "1") = Right 1
  validate (Form "2") = Left $ S.singleton "2"
  validate _ = Left $ S.singleton "3"

type V3 = Validated Bool Int (Field "t" (InputField TextInput))
instance Validation V3 where
  validate (Form "1") = Right 1
  validate _ = Left $ S.singleton True

type U1 = Unvalidated Bool (Field "t" (InputField TextInput))
instance Validation U1 where
  validate (Form "1") = True
  validate _ = False
type U2 = Unvalidated Bool V1
instance Validation U2 where
  validate (Form "1") = True
  validate _ = False
type U3 = Unvalidated Bool (Field "t" (EnumField Int))
instance Validation U3 where
  validate (Form 1) = True
  validate _ = False

type P1 = Field "1" (EnumField Int)
      :+: Field "2" (InputField TextInput)
      :+: Field "3" (InputField TextInput)

type P2 = Field "1" (EnumField Int)
      :+: Field "2" (InputField TextInput)
      :+: Field "3" (EnumField Char)

-- Tests for constructing forms ------------------------------------------------

construction :: Spec
construction = describe "Construction" $ do

    it "named text error field fails" $ do
      False `shouldBe` False
      shouldNotTypecheck (Form "test" :: Form Err N1)
    it "unvalidated form error field fails" $ do
      shouldNotTypecheck (Form "test" :: Form Err U1)
    it "unvalidated pair error form fails" $ do
      shouldNotTypecheck (Form ("test" :+: "test") :: Form Err (N1 :+: N2))

-- Tests for Form newtype ------------------------------------------------------

formNewtype :: Spec
formNewtype = describe "Form newtype" $ do

  describe "Derived instances" $ do
    it "Eq instance" $ do
      (Form "1" :: Form Raw N1) `shouldBe` Form "1"
      (Form "1" :: Form Raw N1) `shouldNotBe` Form "2"

  describe "Monoid instance" $ do
    it "mempty" $ do
      Form mempty `shouldBe` (mempty :: Form Raw N1)
    it "left identity" $ do
      property $ \a -> Form mempty <> Form (pack a)
                    == (Form (pack a) :: Form Raw N1)
    it "right identity" $ do
      property $ \a -> Form (pack a) <> Form mempty
                    == (Form (pack a) :: Form Raw N1)
    it "associativity" $ do
      property $ \a b c -> (Form (pack a) <> Form (pack b)) <> Form (pack c)
        == Form (pack a) <> (Form (pack b) <> (Form (pack c) :: Form Raw N1))

  describe "Show instance" $ do
    it "works as expected" $ do
      show (Form "1" :: Form Raw N1) `shouldBe` "Form \"1\""
      show (Form ("1" :+: "2" :+: "3") :: Form Raw P1) `shouldBe`
        "Form (\"1\" :+: \"2\" :+: \"3\")"

pair :: Spec
pair = describe "Pair combinator" $ do

  describe "Derived instances" $ do
    it "Eq instance" $ do
      1 :+: 2 `shouldBe` 1 :+: 2
      1 :+: 2 `shouldNotBe` 1 :+: 3

  describe "Show instance" $ do
    it "shows correctly" $ do
      show (1 :+: 2 :+: 3) `shouldBe` "1 :+: 2 :+: 3"
    it "shows correctly" $ do
      show (1 :+: Just 2 :+: 3) `shouldBe` "1 :+: Just 2 :+: 3"
    it "shows correctly" $ do
      show (Just $ 1 :+: Just 2 :+: 3) `shouldBe` "Just (1 :+: Just 2 :+: 3)"

  describe "Monoid instance" $ do
    it "mempty" $ do
      mempty `shouldBe` (mempty :: [Int]) :+: (mempty :: Maybe String)
    it "left identity" $ do
      property $ \a b -> mempty <> (a :+: b) == (a :: [Int]) :+: (b :: Maybe String)
    it "right identity" $ do
      property $ \a b -> (a :+: b) <> mempty == (a :: [Int]) :+: (b :: Maybe String)
    it "associativity" $ do
      property $ \a a' b b' c c'
        -> a :+: a' <> (b :+: b' <> c :+: c')
        == (a :+: a' <> b :+: b') <> (c :: [Int]) :+: (c' :: Maybe String)

  describe "Checked Monoid instance" $ do
    it "mempty" $ do
      (mempty :: Checked String) `shouldBe` Unchecked
    it "left identity" $ do
      property $ \a -> mempty <> Checked a
                    == Checked (a :: String)
    it "right identity" $ do
      property $ \a -> Checked a <> mempty
                    == Checked (a :: String)
    it "associativity" $ do
      property $ \a b c -> (Checked a <> Checked b) <> Checked c
        == Checked a <> (Checked b <> Checked (c :: String))

-- Tests for EnumError ---------------------------------------------------------

enumError :: Spec
enumError = describe "EnumError" $ do

  describe "Derived instances" $ do
    it "Eq instance" $ do
      EnumReadFailed `shouldBe` EnumReadFailed
      EnumReadFailed /= EnumReadFailed `shouldBe` False
    it "Ord instance" $ do
      EnumReadFailed > EnumReadFailed `shouldBe` False
      EnumReadFailed < EnumReadFailed `shouldBe` False
      EnumReadFailed <= EnumReadFailed `shouldBe` True
      EnumReadFailed >= EnumReadFailed `shouldBe` True
    it "Show instance" $ do
      showList [EnumReadFailed] "" `shouldBe` show [EnumReadFailed]

-- Tests for validating forms --------------------------------------------------

validation :: Spec
validation = describe "Validation" $ do

  describe "Pair validation, left validated, pair of unvalidated" $ do
    it "validates and passes" $ do
      validateAll (Form $ "1" :+: "2" :+: "3" :: Form Raw P1) `shouldBe`
        Right (Form $ 1 :+: "2" :+: "3")
    it "validates and fails" $ do
      validateAll (Form $ "a" :+: "2" :+: "3" :: Form Raw P1) `shouldBe`
        Left (Form justEnumErr)

  describe "Pair validation, left validated, pair of unvalidated" $ do
    it "validates and passes" $ do
      validateAll (Form $ "1" :+: "2" :+: "'c'" :: Form Raw P2) `shouldBe`
        Right (Form $ 1 :+: "2" :+: 'c')
    it "validates and fails last" $ do
      validateAll (Form $ "1" :+: "2" :+: "3" :: Form Raw P2) `shouldBe`
        Left (Form $ Checked S.empty :+: justEnumErr)
    it "validates and fails first" $ do
      validateAll (Form $ "a" :+: "2" :+: "'c'" :: Form Raw P2) `shouldBe`
        Left (Form $ justEnumErr :+: Checked S.empty)
    it "validates and fails both" $ do
      validateAll (Form $ "a" :+: "2" :+: "3" :: Form Raw P2) `shouldBe`
        Left (Form $ justEnumErr :+: justEnumErr)

  describe "Fields" $ do
    it "validates correctly for (InputField TextInput)" $ do
      validateAll (Form "test" :: Form Raw N1) `shouldBe`
        Form "test"
    it "validates correctly for HiddenField" $ do
      validateAll (Form "test" :: Form Raw N2) `shouldBe`
        Form "test"
    it "validates and passes for EnumField" $ do
      validateAll (Form "1" :: Form Raw N3) `shouldBe`
        Right (Form 1)
    it "validates and fails for EnumField" $ do
      validateAll (Form "no" :: Form Raw N3) `shouldBe`
        Left (Form justEnumErr)

  describe "Unvalidated parent without sub validation" $ do
    it "validates correctly" $ do
      validateAll (Form "1" :: Form Raw U1) `shouldBe` Form True

  describe "Unvalidated parent with sub validation" $ do
    it "validates and passes" $ do
      validateAll (Form "1" :: Form Raw U2) `shouldBe` Right (Form True)
    it "validates and fails" $ do
      validateAll (Form "3" :: Form Raw U2) `shouldBe`
        Left (Form (Checked (S.singleton False), Checked S.empty))

  describe "Validated parent without sub validation" $ do
    it "validates and passes" $ do
      validateAll (Form "1" :: Form Raw V3) `shouldBe` Right (Form 1)
    it "validates and fails" $ do
      validateAll (Form "2" :: Form Raw V3) `shouldBe`
        Left (Form $ Checked $ S.singleton True)

  describe "Nested validation" $ do
    it "validates and passes" $ do
      validateAll (Form "1" :: Form Raw V2) `shouldBe` Right (Form 1)
    it "validates and fails top level" $ do
      validateAll (Form "2" :: Form Raw V2) `shouldBe`
        Left (Form (Checked (S.singleton "2"), (Checked S.empty, Checked S.empty)))
    it "validates and fails mid level" $ do
      validateAll (Form "3" :: Form Raw V2) `shouldBe`
        Left (Form (Unchecked, (Checked (S.singleton False), Checked S.empty)))
    it "validates and fails lowest level" $ do
      validateAll (Form "a" :: Form Raw V2) `shouldBe`
        Left (Form (Unchecked, (Unchecked, justEnumErr)))

-- Tests for branch validating forms -------------------------------------------

branchValidation :: Spec
branchValidation = describe "Branch validation" $ do

  describe "Unvalidated forms" $ do
    it "should not typecheck" $ do
      shouldNotTypecheck $ validateBranch @(Field "t" (InputField TextInput)) (Form @Raw @U1 "1")

  describe "Validated parent" $ do
    it "validates and passes when referring to total form" $ do
      validateBranch @V2 (Form "1" :: Form Raw V2) `shouldBe`
        Form (Checked S.empty, (Checked S.empty, Checked S.empty))
    it "validates and passes when referring to sub form" $ do
      validateBranch @V1 (Form "1" :: Form Raw V2) `shouldBe`
        Form (Checked S.empty, (Checked S.empty, Checked S.empty))
    it "validates and fails when referring to parent form" $ do
      validateBranch @V2 (Form "2" :: Form Raw V2) `shouldBe`
        Form (Checked (S.singleton "2"), (Checked S.empty, Checked S.empty))
    it "validates and fails when referring to sub form" $ do
      validateBranch @V1 (Form "2" :: Form Raw V2) `shouldBe`
        Form (Checked (S.singleton "2"), (Checked S.empty, Checked S.empty))

  describe "Unvalidated parent of validated sub form" $ do
    it "validates and passes when referring to total form" $ do
      validateBranch @U2 (Form "1" :: Form Raw U2) `shouldBe`
        Form (Checked S.empty, Checked S.empty)
    it "validates and passes when referring to sub form" $ do
      validateBranch @V1 (Form "1" :: Form Raw U2) `shouldBe`
        Form (Checked S.empty, Checked S.empty)
    it "validates and fails when referring to total form" $ do
      validateBranch @U2 (Form "a" :: Form Raw U2) `shouldBe`
        Form (Unchecked, justEnumErr)
    it "validates and fails when referring to sub form" $ do
      validateBranch @V1 (Form "a" :: Form Raw U2) `shouldBe`
        Form (Unchecked, justEnumErr)

  describe "Base enum field" $ do
    it "enum field validates and passes" $ do
      validateBranch @N3 (Form "1" :: Form Raw N3) `shouldBe`
        Form (Checked S.empty)
    it "enum field validates and fails" $ do
      validateBranch @N3 (Form "a" :: Form Raw N3) `shouldBe`
        Form justEnumErr

  describe "Pairs" $ do
    it "validates first field, not touching others" $ do
      validateBranch @(Field "1" (EnumField Int))
        (Form $ "1" :+: "2" :+: "'c'" :: Form Raw P2) `shouldBe`
          Form (Checked S.empty :+: Unchecked)
    it "validates second field, not touching others" $ do
      validateBranch @(Field "2" (InputField TextInput))
        (Form $ "1" :+: "2" :+: "'c'" :: Form Raw P2) `shouldBe`
          Form (Unchecked :+: Unchecked)
    it "validates third field, not touching others" $ do
      validateBranch @(Field "3" (EnumField Char))
        (Form $ "1" :+: "2" :+: "'c'" :: Form Raw P2) `shouldBe`
          Form (Unchecked :+: Checked S.empty)

  describe "Pairs, no errors on right branch" $ do
    it "validates first field, not touching others" $ do
      validateBranch @(Field "1" (EnumField Int))
        (Form $ "1" :+: "2" :+: "3" :: Form Raw P1) `shouldBe`
          Form (Checked S.empty)
    it "validates second field, not touching others" $ do
      validateBranch @(Field "2" (InputField TextInput))
        (Form $ "1" :+: "2" :+: "3" :: Form Raw P1) `shouldBe`
          Form Unchecked
    it "validates third field, not touching others" $ do
      validateBranch @(Field "3" (InputField TextInput))
        (Form $ "1" :+: "2" :+: "3" :: Form Raw P1) `shouldBe`
          Form Unchecked

  describe "Pairs, many errors (for testing emptySetErrors)" $ do
    it "validates whole field and passes" $ do
      validateBranch @P2
        (Form $ "1" :+: "2" :+: "'c'" :: Form Raw P2) `shouldBe`
          Form (Checked S.empty :+: Checked S.empty)

-- Tests for form lenses -------------------------------------------------------

lenses :: Spec
lenses = describe "Lenses" $ do

  describe "Raw unvalidated fields" $ do
    let form :: Form Raw (Unvalidated Int (Field "t" (InputField TextInput)))
        form = Form "1"
    it "view should work for total field" $ do
      form ^. subform @(Unvalidated Int (Field "t" (InputField TextInput))) `shouldBe` "1"
    it "set should work for total field" $ do
      (form & subform @(Unvalidated Int (Field "t" (InputField TextInput))) .~ "2")
        `shouldBe` Form "2"
    it "view should work for sub field" $ do
      form ^. subform @(Field "t" (InputField TextInput)) `shouldBe` "1"
    it "set should work for sub field" $ do
      (form & subform @(Field "t" (InputField TextInput)) .~ "2") `shouldBe` Form "2"

  describe "Raw validated fields" $ do
    let f :: Form Raw (Validated Bool Int (Field "t" (InputField TextInput)))
        f = Form "1"
    it "view" $ do
      f ^. subform @(Field "t" (InputField TextInput)) `shouldBe` "1"
    it "set" $ do
      (f & subform @(Field "t" (InputField TextInput)) .~ "2") `shouldBe` Form "2"
    it "view total" $ do
      f ^. subform @(Validated Bool Int (Field "t" (InputField TextInput))) `shouldBe` "1"
    it "set total" $ do
      (f & subform @(Validated Bool Int (Field "t" (InputField TextInput))) .~ "2")
        `shouldBe` Form "2"

  describe "Hs unvalidated fields" $ do
    let form :: Form Hs (Unvalidated Int (Field "t" (InputField TextInput)))
        form = Form 1
    it "should not typecheck for sub field access" $ do
      shouldNotTypecheck $ form ^. subform @(Field "t" (InputField TextInput))
    it "view should work for total field" $ do
      form ^. subform @(Unvalidated Int (Field "t" (InputField TextInput)))
        `shouldBe` 1
    it "set should work for total field" $ do
      (form & subform @(Unvalidated Int (Field "t" (InputField TextInput))) .~ 2)
        `shouldBe` Form 2

  describe "Err unvalidated fields" $ do
    let form :: Form Err (Unvalidated Int (Field "t" (EnumField Char)))
        form = Form justEnumErr
    it "should not typecheck unvalidated top field access" $ do
      shouldNotTypecheck $ form ^.
        subform @(Field "t" (InputField TextInput)) . _1
    it "view should work for validated sub field" $ do
      form ^. subform @(Field "t" (EnumField Char)) `shouldBe` justEnumErr
    it "set should work for validated sub field" $ do
      (form & subform @(Field "t" (EnumField Char)) .~ Unchecked)
        `shouldBe` Form Unchecked
    it "view should work for total field" $ do
      form ^. subform @(Unvalidated Int (Field "t" (EnumField Char)))
        `shouldBe` justEnumErr
    it "set should work for total field" $ do
      (form & subform @(Unvalidated Int (Field "t" (EnumField Char))) .~ Unchecked)
        `shouldBe` Form Unchecked

  describe "Err validated fields" $ do
    let unvalSub :: Form Err (Validated Bool Int (Field "t" (InputField TextInput)))
        unvalSub = Form err
        err = Checked $ S.singleton False
        err' = Checked $ S.singleton True
        valSub :: Form Err (Validated Bool Int (Field "t" (EnumField Char)))
        valSub = Form (err, justEnumErr)
        valSub' :: Form Err (Validated Bool Int (Field "t" (EnumField Char)))
        valSub' = Form (err, Unchecked)
    it "should not typecheck unvalidated subfield access" $ do
      shouldNotTypecheck $ unvalSub ^. subform @(Field "t" (InputField TextInput))
    it "view should work for validated sub field" $ do
      valSub ^. subform @(Field "t" (EnumField Char)) `shouldBe` justEnumErr
    it "set should work for validated sub field" $ do
      (valSub & subform @(Field "t" (EnumField Char)) .~ Unchecked)
        `shouldBe` valSub'
    it "view should work for total field" $ do
      valSub ^. subform @(Validated Bool Int (Field "t" (EnumField Char)))
        `shouldBe` (err, justEnumErr)
    it "view should work for top field" $ do
      valSub ^. subform @(Validated Bool Int (Field "t" (EnumField Char)))
              . _1 `shouldBe` err
    it "set should work for total field" $ do
      (valSub & subform @(Validated Bool Int (Field "t" (EnumField Char)))
              .~ (err', Unchecked)) `shouldBe` Form (err', Unchecked)
    it "set should work for top field" $ do
      (valSub & subform @(Validated Bool Int (Field "t" (EnumField Char)))
              . _1 .~ err') `shouldBe` Form (err', justEnumErr)

  describe "Raw pairs" $ do
    let form :: Form Raw (Field "1" (InputField TextInput)
            :+: Field "2" (InputField TextInput))
        form = Form $ "1" :+: "2"
    it "view should work on the total field" $ do
      form ^. subform @(Field "1" (InputField TextInput)
                    :+: Field "2" (InputField TextInput))
        `shouldBe` "1" :+: "2"
    it "set should work on the total field" $ do
      (form & subform @(Field "1" (InputField TextInput)
                    :+: Field "2" (InputField TextInput))
        .~ "3" :+: "4")
        `shouldBe` Form ("3" :+: "4")
    it "view should work on the first field" $ do
      form ^. subform @(Field "1" (InputField TextInput)) `shouldBe` "1"
    it "set should work on the first field" $ do
      (form & subform @(Field "1" (InputField TextInput)) .~ "A")
        `shouldBe` Form ("A" :+: "2")
    it "view should work on the second field" $ do
      form ^. subform @(Field "2" (InputField TextInput)) `shouldBe` "2"
    it "set should work on the second field" $ do
      (form & subform @(Field "2" (InputField TextInput)) .~ "A")
        `shouldBe` Form ("1" :+: "A")

  describe "Hs pairs" $ do
    let form :: Form Hs (Field "1" (EnumField Bool) :+: Field "2" (InputField TextInput))
        form = Form $ True :+: "2"
    it "view should work on the total field" $ do
      form ^. subform @(Field "1" (EnumField Bool) :+: Field "2" (InputField TextInput))
        `shouldBe` True :+: "2"
    it "set should work on the total field" $ do
      (form & subform @(Field "1" (EnumField Bool) :+: Field "2" (InputField TextInput))
            .~ False :+: "4") `shouldBe` Form (False :+: "4")
    it "view should work on the first field" $ do
      form ^. subform @(Field "1" (EnumField Bool)) `shouldBe` True
    it "set should work on the first field" $ do
      (form & subform @(Field "1" (EnumField Bool)) .~ False)
        `shouldBe` Form (False :+: "2")
    it "view should work on the second field " $ do
      form ^. subform @(Field "2" (InputField TextInput)) `shouldBe` "2"
    it "set should work on the second field" $ do
      (form & subform @(Field "2" (InputField TextInput)) .~ "A")
        `shouldBe` Form (True :+: "A")

  describe "Err pairs, both errors" $ do
    let form :: Form Err (Field "1" (EnumField Bool)
                          :+: Validated Int Text (Field "2" (InputField TextInput)))
        form = Form $ justEnumErr :+: intErr
        intErr = Checked (S.singleton 1)
    it "view should work on the total field" $ do
      form ^. subform @(Field "1" (EnumField Bool)
                    :+: Validated Int Text (Field "2" (InputField TextInput))) `shouldBe`
        justEnumErr :+: intErr
    it "set should work on the total field" $ do
      (form & subform @(Field "1" (EnumField Bool)
                    :+: Validated Int Text (Field "2" (InputField TextInput)))
            .~ Unchecked :+: Checked S.empty) `shouldBe`
        Form (Unchecked :+: Checked S.empty)
    it "view should work on the first field" $ do
      form ^. subform @(Field "1" (EnumField Bool)) `shouldBe` justEnumErr
    it "set should work on the first field" $ do
      (form & subform @(Field "1" (EnumField Bool)) .~ Unchecked) `shouldBe`
        Form (Unchecked :+: intErr)
    it "view should work on the second field" $ do
      form ^. subform @(Validated Int Text (Field "2" (InputField TextInput)))
        `shouldBe` intErr
    it "set should work on the second field" $ do
      (form & subform @(Validated Int Text (Field "2" (InputField TextInput)))
        .~ Unchecked)
        `shouldBe` Form (justEnumErr :+: Unchecked)

  describe "Err pairs, first errors" $ do
    let form :: Form Err (Field "1" (EnumField Bool) :+: Field "2" (InputField TextInput))
        form = Form justEnumErr
    it "should not typecheck on second field access" $ do
      shouldNotTypecheck $ form ^. subform @(Field "2" (InputField TextInput))
    it "view should work on the first field" $ do
      form ^. subform @(Field "1" (EnumField Bool)) `shouldBe` justEnumErr
    it "set should work on the first field" $ do
      (form & subform @(Field "1" (EnumField Bool)) .~ Unchecked) `shouldBe`
        Form Unchecked

  describe "Err pairs, second errors" $ do
    let form :: Form Err (Field "1" (InputField TextInput) :+: Field "2" (EnumField Bool))
        form = Form justEnumErr
    it "should not typecheck on first field access" $ do
      shouldNotTypecheck $ form ^. subform @(Field "1" (InputField TextInput))
    it "view should work on the second field" $ do
      form ^. subform @(Field "2" (EnumField Bool)) `shouldBe` justEnumErr
    it "set should work on the second field" $ do
      (form & subform @(Field "2" (EnumField Bool)) .~ Unchecked) `shouldBe`
        Form Unchecked

