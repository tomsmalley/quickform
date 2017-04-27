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

justEnumErr :: Maybe (S.Set EnumError)
justEnumErr = Just $ S.singleton EnumReadFailed

type N1 = Named "T" TextField
type N2 = Named "H" HiddenField
type N3 = Named "E" (EnumField Int)

type V1 = Validated Bool Text (Named "e" (EnumField Int))
type V2 = Validated String Int V1
instance Validation V1 where
  validate (Form 1) = Valid "1"
  validate (Form 2) = Valid "2"
  validate _ = Invalid $ S.singleton False
instance Validation V2 where
  validate (Form "1") = Valid 1
  validate (Form "2") = Invalid $ S.singleton "2"
  validate _ = Invalid $ S.singleton "3"

type V3 = Validated Bool Int (Named "t" TextField)
instance Validation V3 where
  validate (Form "1") = Valid 1
  validate _ = Invalid $ S.singleton True

type U1 = Unvalidated Bool (Named "t" TextField)
instance Validation U1 where
  validate (Form "1") = True
  validate _ = False
type U2 = Unvalidated Bool V1
instance Validation U2 where
  validate (Form "1") = True
  validate _ = False
type U3 = Unvalidated Bool (Named "t" (EnumField Int))
instance Validation U3 where
  validate (Form 1) = True
  validate _ = False

type P1 = Named "1" (EnumField Int)
      :+: Named "2" TextField
      :+: Named "3" TextField

type P2 = Named "1" (EnumField Int)
      :+: Named "2" TextField
      :+: Named "3" (EnumField Char)

-- Tests for constructing forms ------------------------------------------------

construction :: Spec
construction = describe "Construction" $ do

    it "named text error field fails" $ do
      shouldNotTypecheck (Form "test" :: Form Err N1)
    it "unvalidated form error field fails" $ do
      shouldNotTypecheck (Form "test" :: Form Err U1)
    it "unvalidated pair error form fails" $ do
      shouldNotTypecheck (Form ("test" :*: "test") :: Form Err (N1 :+: N2))

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
      show (Form ("1" :*: "2" :*: "3") :: Form Raw P1) `shouldBe`
        "Form (\"1\" :*: \"2\" :*: \"3\")"

pair :: Spec
pair = describe "Pair combinator" $ do

  describe "Derived instances" $ do
    it "Eq instance" $ do
      1 :*: 2 `shouldBe` 1 :*: 2
      1 :*: 2 `shouldNotBe` 1 :*: 3

  describe "Show instance" $ do
    it "shows correctly" $ do
      show (1 :*: 2 :*: 3) `shouldBe` "1 :*: 2 :*: 3"
    it "shows correctly" $ do
      show (1 :*: Just 2 :*: 3) `shouldBe` "1 :*: Just 2 :*: 3"
    it "shows correctly" $ do
      show (Just $ 1 :*: Just 2 :*: 3) `shouldBe` "Just (1 :*: Just 2 :*: 3)"

  describe "Monoid instance" $ do
    it "mempty" $ do
      mempty `shouldBe` (mempty :: [Int]) :*: (mempty :: Maybe Char)
    it "left identity" $ do
      property $ \a b -> mempty <> (a :*: b) == (a :: [Int]) :*: (b :: Maybe Char)
    it "right identity" $ do
      property $ \a b -> (a :*: b) <> mempty == (a :: [Int]) :*: (b :: Maybe Char)
    it "associativity" $ do
      property $ \a a' b b' c c'
        -> a :*: a' <> (b :*: b' <> c :*: c')
        == (a :*: a' <> b :*: b') <> (c :: [Int]) :*: (c' :: Maybe String)

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

  -- These are just to get 100% coverage
  describe "Derived instances of Validate" $ do
    it "Eq instance" $ do
      Valid 1 `shouldBe` Valid 1
      Valid 1 `shouldNotBe` Valid 2
    it "Show instance" $ do
      showList [Valid 1] "" `shouldBe` show [Valid 1]

  describe "Pair validation, left validated, pair of unvalidated" $ do
    it "validates and passes" $ do
      validateAll (Form $ "1" :*: "2" :*: "3" :: Form Raw P1) `shouldBe`
        Right (Form $ 1 :*: "2" :*: "3")
    it "validates and fails" $ do
      validateAll (Form $ "a" :*: "2" :*: "3" :: Form Raw P1) `shouldBe`
        Left (Form justEnumErr)

  describe "Pair validation, left validated, pair of unvalidated" $ do
    it "validates and passes" $ do
      validateAll (Form $ "1" :*: "2" :*: "'c'" :: Form Raw P2) `shouldBe`
        Right (Form $ 1 :*: "2" :*: 'c')
    it "validates and fails last" $ do
      validateAll (Form $ "1" :*: "2" :*: "3" :: Form Raw P2) `shouldBe`
        Left (Form $ Just S.empty :*: justEnumErr)
    it "validates and fails first" $ do
      validateAll (Form $ "a" :*: "2" :*: "'c'" :: Form Raw P2) `shouldBe`
        Left (Form $ justEnumErr :*: Just S.empty)
    it "validates and fails both" $ do
      validateAll (Form $ "a" :*: "2" :*: "3" :: Form Raw P2) `shouldBe`
        Left (Form $ justEnumErr :*: justEnumErr)

  describe "Named fields" $ do
    it "validates correctly for TextField" $ do
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
        Left (Form (Just (S.singleton False), Just S.empty))

  describe "Validated parent without sub validation" $ do
    it "validates and passes" $ do
      validateAll (Form "1" :: Form Raw V3) `shouldBe` Right (Form 1)
    it "validates and fails" $ do
      validateAll (Form "2" :: Form Raw V3) `shouldBe`
        Left (Form $ Just $ S.singleton True)

  describe "Nested validation" $ do
    it "validates and passes" $ do
      validateAll (Form "1" :: Form Raw V2) `shouldBe` Right (Form 1)
    it "validates and fails top level" $ do
      validateAll (Form "2" :: Form Raw V2) `shouldBe`
        Left (Form (Just (S.singleton "2"), (Just S.empty, Just S.empty)))
    it "validates and fails mid level" $ do
      validateAll (Form "3" :: Form Raw V2) `shouldBe`
        Left (Form (Nothing, (Just (S.singleton False), Just S.empty)))
    it "validates and fails lowest level" $ do
      validateAll (Form "a" :: Form Raw V2) `shouldBe`
        Left (Form (Nothing, (Nothing, justEnumErr)))

-- Tests for branch validating forms -------------------------------------------

branchValidation :: Spec
branchValidation = describe "Branch validation" $ do

  describe "Unvalidated forms" $ do
    it "should not typecheck" $ do
      shouldNotTypecheck $ validateBranch @(Named "t" TextField) (Form @Raw @U1 "1")

  describe "Validated parent" $ do
    it "validates and passes when referring to total form" $ do
      validateBranch @V2 (Form "1" :: Form Raw V2) `shouldBe`
        Form (Just S.empty, (Just S.empty, Just S.empty))
    it "validates and passes when referring to sub form" $ do
      validateBranch @V1 (Form "1" :: Form Raw V2) `shouldBe`
        Form (Just S.empty, (Just S.empty, Just S.empty))
    it "validates and fails when referring to parent form" $ do
      validateBranch @V2 (Form "2" :: Form Raw V2) `shouldBe`
        Form (Just (S.singleton "2"), (Just S.empty, Just S.empty))
    it "validates and fails when referring to sub form" $ do
      validateBranch @V1 (Form "2" :: Form Raw V2) `shouldBe`
        Form (Just (S.singleton "2"), (Just S.empty, Just S.empty))

  describe "Unvalidated parent of validated sub form" $ do
    it "validates and passes when referring to total form" $ do
      validateBranch @U2 (Form "1" :: Form Raw U2) `shouldBe`
        Form (Just S.empty, Just S.empty)
    it "validates and passes when referring to sub form" $ do
      validateBranch @V1 (Form "1" :: Form Raw U2) `shouldBe`
        Form (Just S.empty, Just S.empty)
    it "validates and fails when referring to total form" $ do
      validateBranch @U2 (Form "a" :: Form Raw U2) `shouldBe`
        Form (Nothing, justEnumErr)
    it "validates and fails when referring to sub form" $ do
      validateBranch @V1 (Form "a" :: Form Raw U2) `shouldBe`
        Form (Nothing, justEnumErr)

  describe "Base enum field" $ do
    it "enum field validates and passes" $ do
      validateBranch @N3 (Form "1" :: Form Raw N3) `shouldBe`
        Form (Just S.empty)
    it "enum field validates and fails" $ do
      validateBranch @N3 (Form "a" :: Form Raw N3) `shouldBe`
        Form justEnumErr

  describe "Pairs" $ do
    it "validates first field, not touching others" $ do
      validateBranch @(Named "1" (EnumField Int))
        (Form $ "1" :*: "2" :*: "'c'" :: Form Raw P2) `shouldBe`
          Form (Just S.empty :*: Nothing)
    it "validates second field, not touching others" $ do
      validateBranch @(Named "2" TextField)
        (Form $ "1" :*: "2" :*: "'c'" :: Form Raw P2) `shouldBe`
          Form (Nothing :*: Nothing)
    it "validates third field, not touching others" $ do
      validateBranch @(Named "3" (EnumField Char))
        (Form $ "1" :*: "2" :*: "'c'" :: Form Raw P2) `shouldBe`
          Form (Nothing :*: Just S.empty)

  describe "Pairs, no errors on right branch" $ do
    it "validates first field, not touching others" $ do
      validateBranch @(Named "1" (EnumField Int))
        (Form $ "1" :*: "2" :*: "3" :: Form Raw P1) `shouldBe`
          Form (Just S.empty)
    it "validates second field, not touching others" $ do
      validateBranch @(Named "2" TextField)
        (Form $ "1" :*: "2" :*: "3" :: Form Raw P1) `shouldBe`
          Form Nothing
    it "validates third field, not touching others" $ do
      validateBranch @(Named "3" TextField)
        (Form $ "1" :*: "2" :*: "3" :: Form Raw P1) `shouldBe`
          Form Nothing

  describe "Pairs, many errors (for testing emptySetErrors)" $ do
    it "validates whole field and passes" $ do
      validateBranch @P2
        (Form $ "1" :*: "2" :*: "'c'" :: Form Raw P2) `shouldBe`
          Form (Just S.empty :*: Just S.empty)

-- Tests for form lenses -------------------------------------------------------

lenses :: Spec
lenses = describe "Lenses" $ do

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
        form = Form justEnumErr
    it "should not typecheck unvalidated top field access" $ do
      shouldNotTypecheck $ form ^. subform @(Named "t" TextField) . _1
    it "view should work for validated sub field" $ do
      form ^. subform @(Named "t" (EnumField Char)) `shouldBe` justEnumErr
    it "set should work for validated sub field" $ do
      (form & subform @(Named "t" (EnumField Char)) .~ Nothing)
        `shouldBe` Form Nothing
    it "view should work for total field" $ do
      form ^. subform @(Unvalidated Int (Named "t" (EnumField Char)))
        `shouldBe` justEnumErr
    it "set should work for total field" $ do
      (form & subform @(Unvalidated Int (Named "t" (EnumField Char))) .~ Nothing)
        `shouldBe` Form Nothing

  describe "Err validated fields" $ do
    let unvalSub :: Form Err (Validated Bool Int (Named "t" TextField))
        unvalSub = Form err
        err = Just $ S.singleton False
        err' = Just $ S.singleton True
        valSub :: Form Err (Validated Bool Int (Named "t" (EnumField Char)))
        valSub = Form (err, justEnumErr)
        valSub' :: Form Err (Validated Bool Int (Named "t" (EnumField Char)))
        valSub' = Form (err, Nothing)
    it "should not typecheck unvalidated subfield access" $ do
      shouldNotTypecheck $ unvalSub ^. subform @(Named "t" TextField)
    it "view should work for validated sub field" $ do
      valSub ^. subform @(Named "t" (EnumField Char)) `shouldBe` justEnumErr
    it "set should work for validated sub field" $ do
      (valSub & subform @(Named "t" (EnumField Char)) .~ Nothing)
        `shouldBe` valSub'
    it "view should work for total field" $ do
      valSub ^. subform @(Validated Bool Int (Named "t" (EnumField Char)))
        `shouldBe` (err, justEnumErr)
    it "view should work for top field" $ do
      valSub ^. subform @(Validated Bool Int (Named "t" (EnumField Char)))
              . _1 `shouldBe` err
    it "set should work for total field" $ do
      (valSub & subform @(Validated Bool Int (Named "t" (EnumField Char)))
              .~ (err', Nothing)) `shouldBe` Form (err', Nothing)
    it "set should work for top field" $ do
      (valSub & subform @(Validated Bool Int (Named "t" (EnumField Char)))
              . _1 .~ err') `shouldBe` Form (err', justEnumErr)

  describe "Raw pairs" $ do
    let form :: Form Raw (Named "1" TextField :+: Named "2" TextField)
        form = Form $ "1" :*: "2"
    it "view should work on the total field" $ do
      form ^. subform @(Named "1" TextField :+: Named "2" TextField)
        `shouldBe` "1" :*: "2"
    it "set should work on the total field" $ do
      (form & subform @(Named "1" TextField :+: Named "2" TextField)
        .~ "3" :*: "4")
        `shouldBe` Form ("3" :*: "4")
    it "view should work on the first field" $ do
      form ^. subform @(Named "1" TextField) `shouldBe` "1"
    it "set should work on the first field" $ do
      (form & subform @(Named "1" TextField) .~ "A")
        `shouldBe` Form ("A" :*: "2")
    it "view should work on the second field" $ do
      form ^. subform @(Named "2" TextField) `shouldBe` "2"
    it "set should work on the second field" $ do
      (form & subform @(Named "2" TextField) .~ "A")
        `shouldBe` Form ("1" :*: "A")

  describe "Hs pairs" $ do
    let form :: Form Hs (Named "1" (EnumField Bool) :+: Named "2" TextField)
        form = Form $ True :*: "2"
    it "view should work on the total field" $ do
      form ^. subform @(Named "1" (EnumField Bool) :+: Named "2" TextField)
        `shouldBe` True :*: "2"
    it "set should work on the total field" $ do
      (form & subform @(Named "1" (EnumField Bool) :+: Named "2" TextField)
            .~ False :*: "4") `shouldBe` Form (False :*: "4")
    it "view should work on the first field" $ do
      form ^. subform @(Named "1" (EnumField Bool)) `shouldBe` True
    it "set should work on the first field" $ do
      (form & subform @(Named "1" (EnumField Bool)) .~ False)
        `shouldBe` Form (False :*: "2")
    it "view should work on the second field " $ do
      form ^. subform @(Named "2" TextField) `shouldBe` "2"
    it "set should work on the second field" $ do
      (form & subform @(Named "2" TextField) .~ "A")
        `shouldBe` Form (True :*: "A")

  describe "Err pairs, both errors" $ do
    let form :: Form Err (Named "1" (EnumField Bool)
                          :+: Validated Int Text (Named "2" TextField))
        form = Form $ justEnumErr :*: intErr
        intErr = Just (S.singleton 1)
    it "view should work on the total field" $ do
      form ^. subform @(Named "1" (EnumField Bool)
                    :+: Validated Int Text (Named "2" TextField)) `shouldBe`
        justEnumErr :*: intErr
    it "set should work on the total field" $ do
      (form & subform @(Named "1" (EnumField Bool)
                    :+: Validated Int Text (Named "2" TextField))
            .~ Nothing :*: Just S.empty) `shouldBe`
        Form (Nothing :*: Just S.empty)
    it "view should work on the first field" $ do
      form ^. subform @(Named "1" (EnumField Bool)) `shouldBe` justEnumErr
    it "set should work on the first field" $ do
      (form & subform @(Named "1" (EnumField Bool)) .~ Nothing) `shouldBe`
        Form (Nothing :*: intErr)
    it "view should work on the second field" $ do
      form ^. subform @(Validated Int Text (Named "2" TextField)) `shouldBe`
        intErr
    it "set should work on the second field" $ do
      (form & subform @(Validated Int Text (Named "2" TextField)) .~ Nothing)
        `shouldBe` Form (justEnumErr :*: Nothing)

  describe "Err pairs, first errors" $ do
    let form :: Form Err (Named "1" (EnumField Bool) :+: Named "2" TextField)
        form = Form justEnumErr
    it "should not typecheck on second field access" $ do
      shouldNotTypecheck $ form ^. subform @(Named "2" TextField)
    it "view should work on the first field" $ do
      form ^. subform @(Named "1" (EnumField Bool)) `shouldBe` justEnumErr
    it "set should work on the first field" $ do
      (form & subform @(Named "1" (EnumField Bool)) .~ Nothing) `shouldBe`
        Form Nothing

  describe "Err pairs, second errors" $ do
    let form :: Form Err (Named "1" TextField :+: Named "2" (EnumField Bool))
        form = Form justEnumErr
    it "should not typecheck on first field access" $ do
      shouldNotTypecheck $ form ^. subform @(Named "1" TextField)
    it "view should work on the second field" $ do
      form ^. subform @(Named "2" (EnumField Bool)) `shouldBe` justEnumErr
    it "set should work on the second field" $ do
      (form & subform @(Named "2" (EnumField Bool)) .~ Nothing) `shouldBe`
        Form Nothing

