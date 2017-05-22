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

t :: a -> Touched a
t = Touched

justEnumErr :: Touched (S.Set EnumError)
justEnumErr = Touched $ S.singleton EnumReadFailed

type N1 = Field NoLabel "T" (InputField TextInput)
type N2 = Field NoLabel "H" (InputField HiddenInput)
type N3 = Field NoLabel "E" (EnumField Int)

type V1 = Validated Bool Text (Field NoLabel "e" (EnumField Int))
type V2 = Validated String Int V1
instance Validation V1 where
  validate (Form 1) = Right "1"
  validate (Form 2) = Right "2"
  validate _ = Left $ S.singleton False
instance Validation V2 where
  validate (Form "1") = Right 1
  validate (Form "2") = Left $ S.singleton "2"
  validate _ = Left $ S.singleton "3"

type V3 = Validated Bool Int (Field NoLabel "t" (InputField TextInput))
instance Validation V3 where
  validate (Form "1") = Right 1
  validate _ = Left $ S.singleton True

type V4F1 = Field NoLabel "1" (InputField TextInput)
type V4F2 = Field NoLabel "2" (InputField TextInput)
type V4 = Validated Int Bool (V4F1 :+: V4F2)
instance Validation V4 where
  validate (Form ("1" :+: "2")) = Right True
  validate (Form ("1" :+: _)) = Left $ S.singleton 2
  validate (Form (_ :+: "2")) = Left $ S.singleton 1
  validate (Form (_ :+: _)) = Left $ S.singleton 12

type V5F1 = Field NoLabel "1" (EnumField Int)
type V5F2 = Field NoLabel "2" (EnumField Char)
type V5 = Validated Int Bool (V5F1 :+: V5F2)
instance Validation V5 where
  validate (Form (1 :+: 'b')) = Right True
  validate (Form (1 :+: _)) = Left $ S.singleton 2
  validate (Form (_ :+: 'b')) = Left $ S.singleton 1
  validate (Form (_ :+: _)) = Left $ S.singleton 12


type U1 = Unvalidated Bool (Field NoLabel "t" (InputField TextInput))
instance Validation U1 where
  validate (Form "1") = True
  validate _ = False
type U2 = Unvalidated Bool V1
instance Validation U2 where
  validate (Form "1") = True
  validate _ = False
type U3 = Unvalidated Bool (Field NoLabel "t" (EnumField Int))
instance Validation U3 where
  validate (Form 1) = True
  validate _ = False

type P1 = Field NoLabel "1" (EnumField Int)
      :+: Field NoLabel "2" (InputField TextInput)
      :+: Field NoLabel "3" (InputField TextInput)

type P2 = Field NoLabel "1" (EnumField Int)
      :+: Field NoLabel "2" (InputField TextInput)
      :+: Field NoLabel "3" (EnumField Char)

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
      (Form (t "1") :: Form Raw N1) `shouldBe` Form (t "1")
      (Form (t "1") :: Form Raw N1) `shouldNotBe` Form (t "2")

  let tpack = Touched . pack

  describe "Monoid instance" $ do
    it "mempty" $ do
      Form mempty `shouldBe` (mempty :: Form Raw N1)
    it "left identity" $ do
      property $ \a -> Form mempty <> Form (tpack a)
                    == (Form (tpack a) :: Form Raw N1)
    it "right identity" $ do
      property $ \a -> Form (tpack a) <> Form mempty
                    == (Form (tpack a) :: Form Raw N1)
    it "associativity" $ do
      property $ \a b c -> (Form (tpack a) <> Form (tpack b)) <> Form (tpack c)
        == Form (tpack a) <> (Form (tpack b) <> (Form (tpack c) :: Form Raw N1))

  describe "Show instance" $ do
    it "works as expected" $ do
      show (Form (t "1") :: Form Raw N1) `shouldBe` "Form (Touched \"1\")"
      show (Form ((t "1") :+: (t "2") :+: (t "3")) :: Form Raw P1) `shouldBe`
        "Form (Touched \"1\" :+: Touched \"2\" :+: Touched \"3\")"

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

  describe "Touched Monoid instance" $ do
    it "mempty" $ do
      (mempty :: Touched String) `shouldBe` Untouched
    it "left identity" $ do
      property $ \a -> mempty <> Touched a
                    == Touched (a :: String)
    it "right identity" $ do
      property $ \a -> Touched a <> mempty
                    == Touched (a :: String)
    it "associativity" $ do
      property $ \a b c -> (Touched a <> Touched b) <> Touched c
        == Touched a <> (Touched b <> Touched (c :: String))

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
      validateAll (Form $ (t "1") :+: (t "2") :+: (t "3") :: Form Raw P1) `shouldBe`
        Right (Form $ 1 :+: "2" :+: "3")
    it "validates and fails" $ do
      validateAll (Form $ (t "a") :+: (t "2") :+: (t "3") :: Form Raw P1) `shouldBe`
        Left (Form justEnumErr)

  describe "Pair validation, left validated, pair of unvalidated" $ do
    it "validates and passes" $ do
      validateAll (Form $ (t "1") :+: (t "2") :+: (t "'c'") :: Form Raw P2)
        `shouldBe` Right (Form $ 1 :+: "2" :+: 'c')
    it "validates and fails last" $ do
      validateAll (Form $ (t "1") :+: (t "2") :+: (t "3") :: Form Raw P2) `shouldBe`
        Left (Form $ t S.empty :+: justEnumErr)
    it "validates and fails first" $ do
      validateAll (Form $ (t "a") :+: (t "2") :+: (t "'c'") :: Form Raw P2) `shouldBe`
        Left (Form $ justEnumErr :+: t S.empty)
    it "validates and fails both" $ do
      validateAll (Form $ (t "a") :+: (t "2") :+: (t "3") :: Form Raw P2) `shouldBe`
        Left (Form $ justEnumErr :+: justEnumErr)

  describe "Fields" $ do
    it "validates correctly for (InputField TextInput)" $ do
      validateAll (Form (t "test") :: Form Raw N1) `shouldBe`
        Form "test"
    it "validates correctly for HiddenField" $ do
      validateAll (Form (t "test") :: Form Raw N2) `shouldBe`
        Form "test"
    it "validates and passes for EnumField" $ do
      validateAll (Form (t "1") :: Form Raw N3) `shouldBe`
        Right (Form 1)
    it "validates and fails for EnumField" $ do
      validateAll (Form (t "no") :: Form Raw N3) `shouldBe`
        Left (Form justEnumErr)

  describe "Unvalidated parent without sub validation" $ do
    it "validates correctly" $ do
      validateAll (Form (t "1") :: Form Raw U1) `shouldBe` Form True

  describe "Unvalidated parent with sub validation" $ do
    it "validates and passes" $ do
      validateAll (Form (t "1") :: Form Raw U2) `shouldBe` Right (Form True)
    it "validates and fails" $ do
      validateAll (Form (t "3") :: Form Raw U2) `shouldBe`
        Left (Form (t (S.singleton False), t S.empty))

  describe "Validated parent without sub validation" $ do
    it "validates and passes" $ do
      validateAll (Form (t "1") :: Form Raw V3) `shouldBe` Right (Form 1)
    it "validates and fails" $ do
      validateAll (Form (t "2") :: Form Raw V3) `shouldBe`
        Left (Form $ t $ S.singleton True)

  describe "Nested validation" $ do
    it "validates and passes" $ do
      validateAll (Form (t "1") :: Form Raw V2) `shouldBe` Right (Form 1)
    it "validates and fails top level" $ do
      validateAll (Form (t "2") :: Form Raw V2) `shouldBe`
        Left (Form (t (S.singleton "2"), (t S.empty, t S.empty)))
    it "validates and fails mid level" $ do
      validateAll (Form (t "3") :: Form Raw V2) `shouldBe`
        Left (Form (Untouched, (t (S.singleton False), t S.empty)))
    it "validates and fails lowest level" $ do
      validateAll (Form (t "a") :: Form Raw V2) `shouldBe`
        Left (Form (Untouched, (Untouched, justEnumErr)))

-- Tests for branch validating forms -------------------------------------------

branchValidation :: Spec
branchValidation = describe "Branch validation" $ do

  describe "Unvalidated forms" $ do
    it "should not typecheck" $ do
      shouldNotTypecheck $ validateBranch
        @(Field NoLabel "t" (InputField TextInput)) (Form @Raw @U1 (t "1"))

  describe "Validated parent, unvalidated sub forms" $ do
    it "validates and doesn't touch first" $ do
      validateBranch @V4F1 (Form (Untouched :+: Untouched) :: Form Raw V4)
        `shouldBe` Form Untouched
    it "validates and doesn't touch first" $ do
      validateBranch @V4F1 (Form (Touched "1" :+: Untouched) :: Form Raw V4)
        `shouldBe` Form Untouched
    it "validates and doesn't touch first" $ do
      validateBranch @V4F1 (Form (Untouched :+: Touched "2") :: Form Raw V4)
        `shouldBe` Form Untouched
    it "validates and doesn't touch second" $ do
      validateBranch @V4F2 (Form (Untouched :+: Untouched) :: Form Raw V4)
        `shouldBe` Form Untouched
    it "validates and doesn't touch second" $ do
      validateBranch @V4F2 (Form (Touched "1" :+: Untouched) :: Form Raw V4)
        `shouldBe` Form Untouched
    it "validates and doesn't touch second" $ do
      validateBranch @V4F2 (Form (Untouched :+: Touched "2") :: Form Raw V4)
        `shouldBe` Form Untouched
    it "validates and fails" $ do
      validateBranch @V4F1 (Form (Touched "3" :+: Touched "2") :: Form Raw V4)
        `shouldBe` Form (t $ S.singleton 1)
    it "validates and passes" $ do
      validateBranch @V4F1 (Form (Touched "1" :+: Touched "2") :: Form Raw V4)
        `shouldBe` Form (t S.empty)

  describe "Validated parent, validated sub forms" $ do
    it "validates and doesn't touch first" $ do
      validateBranch @V5F1 (Form (Untouched :+: Untouched) :: Form Raw V5)
        `shouldBe` Form (Untouched, justEnumErr :+: Untouched)
    it "validates and doesn't touch first" $ do
      validateBranch @V5F1 (Form (Touched "1" :+: Untouched) :: Form Raw V5)
        `shouldBe` Form (Untouched, t S.empty :+: Untouched)
    it "validates and doesn't touch top, second untouched" $ do
      validateBranch @V5F2 (Form (Untouched :+: Untouched) :: Form Raw V5)
        `shouldBe` Form (Untouched, Untouched :+: justEnumErr)
    it "validates and doesn't touch top, second touched" $ do
      validateBranch @V5F2 (Form (Untouched :+: Touched "2") :: Form Raw V5)
        `shouldBe` Form (Untouched, Untouched :+: justEnumErr)
    it "validates and fails first sub" $ do
      validateBranch @V5F1 (Form (Touched "c" :+: Touched "'b'") :: Form Raw V5)
        `shouldBe` Form (Untouched, justEnumErr :+: t S.empty)
    it "validates and fails" $ do
      validateBranch @V5F1 (Form (Touched "3" :+: Touched "'b'") :: Form Raw V5)
        `shouldBe` Form (t $ S.singleton 1, t S.empty :+: t S.empty)
    it "validates and passes" $ do
      validateBranch @V5F1 (Form (Touched "1" :+: Touched "'b'") :: Form Raw V5)
        `shouldBe` Form (t S.empty, t S.empty :+: t S.empty)

  describe "Validated parent" $ do
    it "validates and passes when referring to total form" $ do
      validateBranch @V2 (Form (t "1") :: Form Raw V2) `shouldBe`
        Form (t S.empty, (t S.empty, t S.empty))
    it "validates and passes when referring to sub form" $ do
      validateBranch @V1 (Form (t "1") :: Form Raw V2) `shouldBe`
        Form (t S.empty, (t S.empty, t S.empty))
    it "validates and fails when referring to parent form" $ do
      validateBranch @V2 (Form (t "2") :: Form Raw V2) `shouldBe`
        Form (t (S.singleton "2"), (t S.empty, t S.empty))
    it "validates and fails when referring to sub form" $ do
      validateBranch @V1 (Form (t "2") :: Form Raw V2) `shouldBe`
        Form (t (S.singleton "2"), (t S.empty, t S.empty))

  describe "Unvalidated parent of validated sub form" $ do
    it "validates and passes when referring to total form" $ do
      validateBranch @U2 (Form (t "1") :: Form Raw U2) `shouldBe`
        Form (t S.empty, t S.empty)
    it "validates and passes when referring to sub form" $ do
      validateBranch @V1 (Form (t "1") :: Form Raw U2) `shouldBe`
        Form (t S.empty, t S.empty)
    it "validates and fails when referring to total form" $ do
      validateBranch @U2 (Form (t "a") :: Form Raw U2) `shouldBe`
        Form (Untouched, justEnumErr)
    it "validates and fails when referring to sub form" $ do
      validateBranch @V1 (Form (t "a") :: Form Raw U2) `shouldBe`
        Form (Untouched, justEnumErr)

  describe "Base enum field" $ do
    it "enum field validates and passes" $ do
      validateBranch @N3 (Form (t "1") :: Form Raw N3) `shouldBe`
        Form (t S.empty)
    it "enum field validates and fails" $ do
      validateBranch @N3 (Form (t "a") :: Form Raw N3) `shouldBe`
        Form justEnumErr

  describe "Pairs" $ do
    it "validates first field, not touching others" $ do
      validateBranch @(Field NoLabel "1" (EnumField Int))
        (Form $ t "1" :+: t "2" :+: t "'c'" :: Form Raw P2) `shouldBe`
          Form (t S.empty :+: Untouched)
    it "validates second field, not touching others" $ do
      validateBranch @(Field NoLabel "2" (InputField TextInput))
        (Form $ t "1" :+: t "2" :+: t "'c'" :: Form Raw P2) `shouldBe`
          Form (Untouched :+: Untouched)
    it "validates third field, not touching others" $ do
      validateBranch @(Field NoLabel "3" (EnumField Char))
        (Form $ t "1" :+: t "2" :+: t "'c'" :: Form Raw P2) `shouldBe`
          Form (Untouched :+: t S.empty)

  describe "Pairs, no errors on right branch" $ do
    it "validates first field, not touching others" $ do
      validateBranch @(Field NoLabel "1" (EnumField Int))
        (Form $ t "1" :+: t "2" :+: t "3" :: Form Raw P1) `shouldBe`
          Form (t S.empty)
    it "validates second field, not touching others" $ do
      validateBranch @(Field NoLabel "2" (InputField TextInput))
        (Form $ t "1" :+: t "2" :+: t "3" :: Form Raw P1) `shouldBe`
          Form Untouched
    it "validates third field, not touching others" $ do
      validateBranch @(Field NoLabel "3" (InputField TextInput))
        (Form $ t "1" :+: t "2" :+: t "3" :: Form Raw P1) `shouldBe`
          Form Untouched

  describe "Pairs, many errors (for testing emptySetErrors)" $ do
    it "validates whole field and passes" $ do
      validateBranch @P2
        (Form $ t "1" :+: t "2" :+: t "'c'" :: Form Raw P2)
          `shouldBe` Form (t S.empty :+: t S.empty)

-- Tests for form lenses -------------------------------------------------------

lenses :: Spec
lenses = describe "Lenses" $ do

  describe "Raw unvalidated fields" $ do
    let form :: Form Raw (Unvalidated Int
                    (Field NoLabel "t" (InputField TextInput)))
        form = Form (Touched "1")
    it "view should work for total field" $ do
      form ^. subform @(Unvalidated Int
        (Field NoLabel "t" (InputField TextInput))) `shouldBe` Touched "1"
    it "set should work for total field" $ do
      (form & subform @(Unvalidated Int
        (Field NoLabel "t" (InputField TextInput))) ??~ "2")
        `shouldBe` Form (Touched "2")
    it "view should work for sub field" $ do
      form ^. subform @(Field NoLabel "t" (InputField TextInput))
      `shouldBe` Touched "1"
    it "set should work for sub field" $ do
      (form & subform @(Field NoLabel "t" (InputField TextInput)) ??~ "2")
      `shouldBe` Form (Touched "2")

  describe "Raw validated fields" $ do
    let f :: Form Raw (Validated Bool Int
                      (Field NoLabel "t" (InputField TextInput)))
        f = Form $ Touched "1"
    it "view" $ do
      f ^. subform @(Field NoLabel "t" (InputField TextInput)) `shouldBe`
        Touched "1"
    it "set" $ do
      (f & subform @(Field NoLabel "t" (InputField TextInput)) ??~ "2")
        `shouldBe` Form (Touched "2")
    it "view total" $ do
      f ^. subform @(Validated Bool Int
        (Field NoLabel "t" (InputField TextInput))) `shouldBe` Touched "1"
    it "set total" $ do
      (f & subform @(Validated Bool Int
        (Field NoLabel "t" (InputField TextInput))) ??~ "2")
        `shouldBe` Form (Touched "2")

  describe "Hs unvalidated fields" $ do
    let form :: Form Hs (Unvalidated Int (Field NoLabel "t" (InputField TextInput)))
        form = Form 1
    it "should not typecheck for sub field access" $ do
      shouldNotTypecheck $ form ^. subform @(Field NoLabel "t" (InputField TextInput))
    it "view should work for total field" $ do
      form ^. subform @(Unvalidated Int (Field NoLabel "t" (InputField TextInput)))
        `shouldBe` 1
    it "set should work for total field" $ do
      (form & subform @(Unvalidated Int (Field NoLabel "t" (InputField TextInput))) .~ 2)
        `shouldBe` Form 2

  describe "Err unvalidated fields" $ do
    let form :: Form Err (Unvalidated Int (Field NoLabel "t" (EnumField Char)))
        form = Form justEnumErr
    it "should not typecheck unvalidated top field access" $ do
      shouldNotTypecheck $ form ^.
        subform @(Field NoLabel "t" (InputField TextInput)) . _1
    it "view should work for validated sub field" $ do
      form ^. subform @(Field NoLabel "t" (EnumField Char)) `shouldBe` justEnumErr
    it "set should work for validated sub field" $ do
      (form & subform @(Field NoLabel "t" (EnumField Char)) .~ Untouched)
        `shouldBe` Form Untouched
    it "view should work for total field" $ do
      form ^. subform @(Unvalidated Int (Field NoLabel "t" (EnumField Char)))
        `shouldBe` justEnumErr
    it "set should work for total field" $ do
      (form & subform @(Unvalidated Int (Field NoLabel "t" (EnumField Char))) .~ Untouched)
        `shouldBe` Form Untouched

  describe "Err validated fields" $ do
    let unvalSub :: Form Err (Validated Bool Int (Field NoLabel "t" (InputField TextInput)))
        unvalSub = Form err
        err = Touched $ S.singleton False
        err' = Touched $ S.singleton True
        valSub :: Form Err (Validated Bool Int (Field NoLabel "t" (EnumField Char)))
        valSub = Form (err, justEnumErr)
        valSub' :: Form Err (Validated Bool Int (Field NoLabel "t" (EnumField Char)))
        valSub' = Form (err, Untouched)
    it "should not typecheck unvalidated subfield access" $ do
      shouldNotTypecheck $ unvalSub ^. subform @(Field NoLabel "t" (InputField TextInput))
    it "view should work for validated sub field" $ do
      valSub ^. subform @(Field NoLabel "t" (EnumField Char)) `shouldBe` justEnumErr
    it "set should work for validated sub field" $ do
      (valSub & subform @(Field NoLabel "t" (EnumField Char)) .~ Untouched)
        `shouldBe` valSub'
    it "view should work for total field" $ do
      valSub ^. subform @(Validated Bool Int (Field NoLabel "t" (EnumField Char)))
        `shouldBe` (err, justEnumErr)
    it "view should work for top field" $ do
      valSub ^. subform @(Validated Bool Int (Field NoLabel "t" (EnumField Char)))
              . _1 `shouldBe` err
    it "set should work for total field" $ do
      (valSub & subform @(Validated Bool Int (Field NoLabel "t" (EnumField Char)))
              .~ (err', Untouched)) `shouldBe` Form (err', Untouched)
    it "set should work for top field" $ do
      (valSub & subform @(Validated Bool Int (Field NoLabel "t" (EnumField Char)))
              . _1 .~ err') `shouldBe` Form (err', justEnumErr)

  describe "Raw pairs" $ do
    let form :: Form Raw (Field NoLabel "1" (InputField TextInput)
            :+: Field NoLabel "2" (InputField TextInput))
        form = Form $ Touched "1" :+: Touched "2"
    it "view should work on the total field" $ do
      form ^. subform @(Field NoLabel "1" (InputField TextInput)
                    :+: Field NoLabel "2" (InputField TextInput))
        `shouldBe` Touched "1" :+: Touched "2"
    it "set should work on the total field" $ do
      (form & subform @(Field NoLabel "1" (InputField TextInput)
                    :+: Field NoLabel "2" (InputField TextInput))
        .~ Touched "3" :+: Touched "4")
        `shouldBe` Form (Touched "3" :+: Touched "4")
    it "view should work on the first field" $ do
      form ^. subform @(Field NoLabel "1" (InputField TextInput)) `shouldBe`
        Touched "1"
    it "set should work on the first field" $ do
      (form & subform @(Field NoLabel "1" (InputField TextInput)) ??~ "A")
        `shouldBe` Form (Touched "A" :+: Touched "2")
    it "view should work on the second field" $ do
      form ^. subform @(Field NoLabel "2" (InputField TextInput)) `shouldBe`
        Touched "2"
    it "set should work on the second field" $ do
      (form & subform @(Field NoLabel "2" (InputField TextInput)) ??~ "A")
        `shouldBe` Form (Touched "1" :+: Touched "A")

  describe "Hs pairs" $ do
    let form :: Form Hs (Field NoLabel "1" (EnumField Bool) :+: Field NoLabel "2" (InputField TextInput))
        form = Form $ True :+: "2"
    it "view should work on the total field" $ do
      form ^. subform @(Field NoLabel "1" (EnumField Bool) :+: Field NoLabel "2" (InputField TextInput))
        `shouldBe` True :+: "2"
    it "set should work on the total field" $ do
      (form & subform @(Field NoLabel "1" (EnumField Bool) :+: Field NoLabel "2" (InputField TextInput))
            .~ False :+: "4") `shouldBe` Form (False :+: "4")
    it "view should work on the first field" $ do
      form ^. subform @(Field NoLabel "1" (EnumField Bool)) `shouldBe` True
    it "set should work on the first field" $ do
      (form & subform @(Field NoLabel "1" (EnumField Bool)) .~ False)
        `shouldBe` Form (False :+: "2")
    it "view should work on the second field " $ do
      form ^. subform @(Field NoLabel "2" (InputField TextInput)) `shouldBe` "2"
    it "set should work on the second field" $ do
      (form & subform @(Field NoLabel "2" (InputField TextInput)) .~ "A")
        `shouldBe` Form (True :+: "A")

  describe "Err pairs, both errors" $ do
    let form :: Form Err (Field NoLabel "1" (EnumField Bool)
                          :+: Validated Int Text (Field NoLabel "2" (InputField TextInput)))
        form = Form $ justEnumErr :+: intErr
        intErr = Touched (S.singleton 1)
    it "view should work on the total field" $ do
      form ^. subform @(Field NoLabel "1" (EnumField Bool)
                    :+: Validated Int Text (Field NoLabel "2" (InputField TextInput))) `shouldBe`
        justEnumErr :+: intErr
    it "set should work on the total field" $ do
      (form & subform @(Field NoLabel "1" (EnumField Bool)
                    :+: Validated Int Text (Field NoLabel "2" (InputField TextInput)))
            .~ Untouched :+: Touched S.empty) `shouldBe`
        Form (Untouched :+: Touched S.empty)
    it "view should work on the first field" $ do
      form ^. subform @(Field NoLabel "1" (EnumField Bool)) `shouldBe` justEnumErr
    it "set should work on the first field" $ do
      (form & subform @(Field NoLabel "1" (EnumField Bool)) .~ Untouched) `shouldBe`
        Form (Untouched :+: intErr)
    it "view should work on the second field" $ do
      form ^. subform @(Validated Int Text (Field NoLabel "2" (InputField TextInput)))
        `shouldBe` intErr
    it "set should work on the second field" $ do
      (form & subform @(Validated Int Text (Field NoLabel "2" (InputField TextInput)))
        .~ Untouched)
        `shouldBe` Form (justEnumErr :+: Untouched)

  describe "Err pairs, first errors" $ do
    let form :: Form Err (Field NoLabel "1" (EnumField Bool) :+: Field NoLabel "2" (InputField TextInput))
        form = Form justEnumErr
    it "should not typecheck on second field access" $ do
      shouldNotTypecheck $ form ^. subform @(Field NoLabel "2" (InputField TextInput))
    it "view should work on the first field" $ do
      form ^. subform @(Field NoLabel "1" (EnumField Bool)) `shouldBe` justEnumErr
    it "set should work on the first field" $ do
      (form & subform @(Field NoLabel "1" (EnumField Bool)) .~ Untouched) `shouldBe`
        Form Untouched

  describe "Err pairs, second errors" $ do
    let form :: Form Err (Field NoLabel "1" (InputField TextInput) :+: Field NoLabel "2" (EnumField Bool))
        form = Form justEnumErr
    it "should not typecheck on first field access" $ do
      shouldNotTypecheck $ form ^. subform @(Field NoLabel "1" (InputField TextInput))
    it "view should work on the second field" $ do
      form ^. subform @(Field NoLabel "2" (EnumField Bool)) `shouldBe` justEnumErr
    it "set should work on the second field" $ do
      (form & subform @(Field NoLabel "2" (EnumField Bool)) .~ Untouched) `shouldBe`
        Form Untouched

