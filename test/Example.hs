{-# LANGUAGE DataKinds, OverloadedStrings, TypeApplications, TypeOperators,
   TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_HADDOCK hide #-}

module Example where

import QuickForm
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)

newtype AName = AName Text deriving Show
type MyNameF = UnvalidatedForm AName :<: NamedForm "name" TextForm
data AgeError = NoParse | InvalidAge | TooOld | TooYoung
  deriving (Eq, Ord, Show)
type MyAgeF = ValidatedForm AgeError (Int, Int)
          :<: YearForm :&: MonthForm
type YearForm = NamedForm "years" TextForm
type MonthForm = NamedForm "months" TextForm

data Job = Plumber | Haskeller | Journalist deriving (Read, Show)
data JobError = NoPlumbers deriving (Eq, Ord, Show)
type MyJobF = ValidatedForm JobError Job :<: NamedForm "job" (EnumForm Job)

type MyForm = MyNameF :&: MyAgeF :&: MyJobF
type MyUForm = MyNameF :&: MyJobF

instance Validation MyNameF where
  validate = AName

instance Validation MyAgeF where
  validate (years :&: months)
    = case (readMaybe @Int (T.unpack years), readMaybe @Int (T.unpack months)) of
      (Nothing, _) -> Invalid $ S.singleton NoParse
      (_, Nothing) -> Invalid $ S.singleton NoParse
      (Just y, Just m) -> checkAge y m
    where
      checkAge y m
        | m > 12 = Invalid $ S.singleton InvalidAge
        | y > 100 = Invalid $ S.singleton TooOld
        | y < 18 = Invalid $ S.singleton TooYoung
        | otherwise = Valid (y, m)

instance Validation MyJobF where
  validate Plumber = Invalid $ S.singleton NoPlumbers
  validate j = Valid j

haskeller :: Form 'Raw MyForm
haskeller = Form $ "Tom" :&: ("24" :&: "6") :&: "Haskeller"

plumber :: Form 'Raw MyForm
plumber = Form $ "Tony" :&: ("48" :&: "2") :&: "Plumber"

nojob :: Form 'Raw MyForm
nojob = Form $ "Robin" :&: ("14" :&: "9") :&: "Unemployed"

vplumb = validateAll plumber
vhaskeller = validateAll haskeller
vnojob = validateAll nojob

uraw :: Form 'Raw MyUForm
uraw = Form $ "Tom" :&: "Haskeller"

myName :: Form 'Raw MyNameF
myName = Form "Tom"

-- Lens error tests
--noerr = vnojob ^. _Left . subform @MyNameF
--nolens = haskeller ^. subform @MyUForm
--nohs = vplumb ^. _Right . subform @YearForm

-- Partial validation error tests
--err1 = validateBranch @MyNameF haskeller -- should work
--err2 = validatePartial @MyNameF uraw -- should fail, form always valid
--err3 = validatePartial @MyAgeF uraw -- should fail, sub not in form
