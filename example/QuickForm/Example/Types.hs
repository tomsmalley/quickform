{-# LANGUAGE GADTs, FlexibleContexts, DeriveGeneric, DataKinds, OverloadedStrings, TypeApplications,
   ScopedTypeVariables, TypeOperators, FlexibleInstances, ApplicativeDo #-}

module QuickForm.Example.Types where

import Data.Aeson hiding (Result(..))
import Data.Char (isAlphaNum, isDigit)
import Data.Default
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import GHC.Generics (Generic)
import qualified Data.Set as S

import Data.Text (Text)
import qualified Data.Text as T
import Data.Either.Validation

failWhen :: Bool -> e -> Validation e ()
failWhen True e = Failure e
failWhen False _ = Success ()

data NullError = NullError

notNull :: Text -> Either NullError Text
notNull t
  | T.null t = Left NullError
  | otherwise = Right t

data Username = Username Text deriving (Eq, Show)

data UsernameError
  = UsernameError_AlreadyExists
  | UsernameError_TooShort
  | UsernameError_TooLong
  | UsernameError_Invalid
  deriving (Eq, Ord, Show, Generic)
instance ToJSON UsernameError
instance FromJSON UsernameError

mkUsername :: Text -> Either UsernameError Username
mkUsername n
  | T.any (not . isAlphaNum) n = Left UsernameError_Invalid
  | T.length n > 12 = Left UsernameError_TooLong
  | T.length n < 3 = Left UsernameError_TooShort
  | otherwise = Right $ Username n

data Phone = Phone Text deriving (Eq, Show)

data PhoneError
  = PhoneError_NotANumber
  | PhoneError_TooShort
  | PhoneError_TooLong
  deriving (Eq, Ord, Show, Generic)
instance ToJSON PhoneError
instance FromJSON PhoneError

mkPhone :: Text -> Either PhoneError Phone
mkPhone n
  | T.any (not . isDigit) n = Left PhoneError_NotANumber
  | T.length n > 12 = Left PhoneError_TooLong
  | T.length n < 5 = Left PhoneError_TooShort
  | otherwise = Right $ Phone n

data Name = Name
  { name_First :: Text
  , name_Middle :: Text
  , name_Last :: Text
  } deriving (Eq, Show)

data NameError
  = NameError_MissingFirstName
  | NameError_MissingLastName
  deriving (Eq, Ord, Show, Generic)
instance ToJSON NameError
instance FromJSON NameError

mkName :: Text -> Text -> Text -> Validation [NameError] Name
mkName f m l = do
  _ <- failWhen (T.null f) [NameError_MissingFirstName]
  _ <- failWhen (T.null l) [NameError_MissingLastName]
  pure $ Name f m l

data Password = Password Text deriving (Eq, Show)

data PasswordError
  = PasswordError_TooShort
  | PasswordError_NeedsNumber
  | PasswordError_DontMatch
  deriving (Eq, Ord, Show, Generic)
instance ToJSON PasswordError
instance FromJSON PasswordError

mkPassword :: Text -> Text -> Validation (NonEmpty PasswordError) Password
mkPassword p1 p2 = do
  _ <- failWhen (T.length p1 < 8) $ pure PasswordError_TooShort
  _ <- failWhen (not $ T.any isDigit p1) $ pure PasswordError_NeedsNumber
  _ <- failWhen (p1 /= p2) $ pure PasswordError_DontMatch
  pure $ Password p1

data Colour
  = Colour_Red
  | Colour_Green
  | Colour_Blue
  deriving (Eq, Ord, Show, Generic)
instance ToJSON Colour
instance FromJSON Colour

data ColourError
  = ColourError_Nonsense
  | ColourError_FavouriteMissing
  | ColourError_LeastFavouriteMissing
  deriving (Eq, Ord, Show, Generic)
instance ToJSON ColourError
instance FromJSON ColourError

checkColours :: Maybe Colour -> Maybe Colour -> Validation (Set ColourError) (Colour, Colour)
checkColours mFavourite mLeastFavourite = do
  favourite <- maybe (Failure $ S.singleton ColourError_FavouriteMissing) Success mFavourite
  leastFavourite <- maybe (Failure $ S.singleton ColourError_LeastFavouriteMissing) Success mLeastFavourite
  _ <- failWhen (fromMaybe False $ (==) <$> mFavourite <*> mLeastFavourite) $ S.singleton ColourError_Nonsense
  pure (favourite, leastFavourite)

data AnimalType
  = AnimalType_Frog
  | AnimalType_Cat
  | AnimalType_Dog
  | AnimalType_Other
  deriving (Eq, Ord, Show, Generic)
instance ToJSON AnimalType
instance FromJSON AnimalType

data Animal = Animal
  { animal_name :: Text
  , animal_type :: AnimalType
  } deriving (Eq, Show)

