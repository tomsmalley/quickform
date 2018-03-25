{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module QuickForm.Example.Forms where

import Data.Default
import Data.List.NonEmpty (NonEmpty)
import Data.Set (Set)
import Data.Text (Text)
import Data.Either.Validation

import QuickForm
import QuickForm.Example.Types

-- Form setup
-- This needs to have nice syntax

type Form_NewUsername = Validated UsernameError Username (Field "new-username" Text)
newUsernameVal :: Validator Form_NewUsername
newUsernameVal = ValidatedVal mkUsername def

type Form_Username = Validated NullError Username (Field "username" Text)
usernameVal :: Validator Form_Username
usernameVal = ValidatedVal (fmap Username . notNull) def

type Form_Phone = Validated PhoneError Phone (Field "phone" Text)
phoneVal :: Validator Form_Phone
phoneVal = ValidatedVal mkPhone def

type Form_NewPassword = Validated (NonEmpty PasswordError) Password
  (Fields [Field "password" Text, Field "password-repeat" Text])
newPasswordVal :: Validator Form_NewPassword
newPasswordVal = ValidatedVal (\(p1 :| p2 :| Nil) -> validationToEither $ mkPassword p1 p2) def

type Form_Name = Validated [NameError] Name
  (Fields [Field "name-first" Text, Field "name-middle" Text, Field "name-last" Text])
nameVal :: Validator Form_Name
nameVal = ValidatedVal (\(f :| n :| l :| Nil) -> validationToEither $ mkName f n l) def

type Form_Colours = Validated (Set ColourError) (Colour, Colour)
  (Fields [Field "colour-favourite" (Maybe Colour), Field "colour-least-favourite" (Maybe Colour)])
coloursVal :: Validator Form_Colours
coloursVal = ValidatedVal (\(favourite :| least :| Nil) -> validationToEither $ checkColours favourite least) def

type Form_Animal = Validated () Animal
  (Fields [Field "animal-name" Text, Field "animal-type" AnimalType])
animalValidator :: Validator Form_Animal
animalValidator = ValidatedVal (\(name :| sort :| Nil) -> pure $ Animal name sort) def

type Form_SignUp = Fields
  [ Form_NewUsername
  , Form_NewPassword
  , Form_Name
  , Form_Phone
  , Form_Animal
  , Form_Colours
  ]
signupValidator :: Validator Form_SignUp
signupValidator
  = FieldsVal $ newUsernameVal :| newPasswordVal :| nameVal
  :| phoneVal :| animalValidator :| coloursVal :| Nil

