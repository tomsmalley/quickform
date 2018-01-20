{-# LANGUAGE DeriveFunctor, DeriveGeneric, TypeFamilies
           , UndecidableInstances #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}

-- | Form wrapper
module QuickForm.Form where

import Data.Kind (Constraint)
import Data.Default
import Control.DeepSeq (NFData)
import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch)
import Data.Aeson.TH
import Data.HashMap.Lazy (HashMap)
import Data.Proxy
import Data.Semigroup (Semigroup(..))
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol, symbolVal)

import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import qualified Data.Set as S

import QuickForm.TypeLevel

-- Touched type ----------------------------------------------------------------

-- | Like 'Maybe' but with a different monoid instance
data Touched a = Untouched | Touched a
  deriving (Eq, Functor, Generic, Read, Show)

instance ToJSON a => ToJSON (Touched a)
instance FromJSON a => FromJSON (Touched a)
instance NFData a => NFData (Touched a)

instance Semigroup (Touched a) where
  Touched a <> _ = Touched a
  _ <> a = a

instance Monoid (Touched a) where
  mempty = Untouched
  mappend = (<>)

touched :: b -> (a -> b) -> Touched a -> b
touched b _ Untouched = b
touched _ f (Touched a) = f a

fromTouched :: a -> Touched a -> a
fromTouched a Untouched = a
fromTouched _ (Touched a) = a

-- Type functions --------------------------------------------------------------

data family FormRaw (q :: QuickForm)
newtype instance FormRaw (Validated e a q) = ValidatedRaw (FormRaw q)
newtype instance FormRaw (Unvalidated a q) = UnvalidatedRaw (FormRaw q)
newtype instance FormRaw (SubForm qs) = FormRaw (TList (Map FormRaw qs))
newtype instance FormRaw (Field n a) = FieldRaw a deriving (Eq, Show, Generic)

deriving instance Eq (FormRaw q) => Eq (FormRaw (Validated e a q))
deriving instance Eq (FormRaw q) => Eq (FormRaw (Unvalidated a q))
deriving instance Eq (TList (Map FormRaw q)) => Eq (FormRaw (SubForm q))

deriving instance Show (FormRaw q) => Show (FormRaw (Validated e a q))
deriving instance Show (FormRaw q) => Show (FormRaw (Unvalidated a q))
deriving instance Show (TList (Map FormRaw q)) => Show (FormRaw (SubForm q))

deriving instance Generic (FormRaw q) => Generic (FormRaw (Validated e a q))
deriving instance Generic (FormRaw q) => Generic (FormRaw (Unvalidated a q))
deriving instance Generic (TList (Map FormRaw q))
  => Generic (FormRaw (SubForm q))

data family FormHs (q :: QuickForm)
newtype instance FormHs (Validated e a q) = ValidatedHs a
  deriving (Show, Eq, Generic)
newtype instance FormHs (Unvalidated a q) = UnvalidatedHs a
  deriving (Show, Eq, Generic)
newtype instance FormHs (Field n a) = FieldHs a
  deriving (Show, Eq, Generic)
newtype instance FormHs (SubForm qs) = FormHs (TList (Map FormHs qs))

deriving instance Show (TList (Map FormHs q)) => Show (FormHs (SubForm q))
deriving instance Eq (TList (Map FormHs qs)) => Eq (FormHs (SubForm qs))
deriving instance Generic (TList (Map FormHs qs))
  => Generic (FormHs (SubForm qs))


class GetHs q a | q -> a where
  getHs :: FormHs q -> a

instance GetHs (Validated e a q) a where
  getHs (ValidatedHs a) = a
instance GetHs (Unvalidated a q) a where
  getHs (UnvalidatedHs a) = a
instance GetHs (Field n a) a where
  getHs (FieldHs a) = a

data family FormErr (q :: QuickForm)
data instance FormErr (Validated e a q) = ValidatedErr e (FormErr q)
newtype instance FormErr (Unvalidated a q) = UnvalidatedErr (FormErr q)
newtype instance FormErr (SubForm qs) = FormErr (TList (Map FormErr qs))
data instance FormErr (Field n a) = FieldErr deriving (Eq, Show, Generic)

deriving instance (Show e, Show (FormErr q)) => Show (FormErr (Validated e a q))
deriving instance Show (FormErr q) => Show (FormErr (Unvalidated a q))
deriving instance Show (TList (Map FormErr qs)) => Show (FormErr (SubForm qs))

deriving instance (Eq e, Eq (FormErr q)) => Eq (FormErr (Validated e a q))
deriving instance Eq (FormErr q) => Eq (FormErr (Unvalidated a q))
deriving instance Eq (TList (Map FormErr qs)) => Eq (FormErr (SubForm qs))

deriving instance (Generic e, Generic (FormErr q))
  => Generic (FormErr (Validated e a q))
deriving instance Generic (FormErr q) => Generic (FormErr (Unvalidated a q))
deriving instance Generic (TList (Map FormErr qs))
  => Generic (FormErr (SubForm qs))

instance (Default e, Generic e, Generic (FormErr q), Default (FormErr q))
  => Default (FormErr (Validated e a q))
instance (Default (FormErr q), Generic (FormErr q))
  => Default (FormErr (Unvalidated a q))
instance (Generic (TList (Map FormErr qs)), Default (TList (Map FormErr qs)))
  => Default (FormErr (SubForm qs))
instance Default (FormErr (Field n a))

--------------------------------------------------------------------------------

class ToHashMap q where
  toHashMap :: FormRaw q -> HashMap Text Value

instance ToHashMap q => ToHashMap (Validated e a q) where
  toHashMap (ValidatedRaw q) = toHashMap q

instance ToHashMap q => ToHashMap (Unvalidated a q) where
  toHashMap (UnvalidatedRaw q) = toHashMap q

instance KnownSymbol n => ToHashMap (Field n Text) where
  toHashMap (FieldRaw value) = HM.singleton name $ String value
    where name = symbolText $ Proxy @n

instance ToHashMap (SubForm '[]) where
  toHashMap (FormRaw Nil) = HM.empty

instance (ToHashMap q, ToHashMap (SubForm qs))
  => ToHashMap (SubForm (q ': qs)) where
    toHashMap (FormRaw (q :| qs))
      = toHashMap q <> toHashMap @(SubForm qs) (FormRaw qs)


instance ToHashMap q => ToJSON (FormRaw q) where
  toJSON q = Object $ toHashMap q

--------------------------------------------------------------------------------

class FromHashMap q where
  fromHashMap :: HashMap Text Value -> Parser (FormRaw q)

instance FromHashMap q => FromHashMap (Validated e a q) where
  fromHashMap = fmap ValidatedRaw . fromHashMap

instance FromHashMap q => FromHashMap (Unvalidated a q) where
  fromHashMap = fmap UnvalidatedRaw . fromHashMap

instance FromHashMap (SubForm '[]) where
  fromHashMap _ = pure $ FormRaw Nil

instance (FromHashMap q, FromHashMap (SubForm qs))
  => FromHashMap (SubForm (q ': qs)) where
  fromHashMap hm = (\r (FormRaw rs) -> FormRaw (r :| rs))
    <$> fromHashMap @q hm <*> fromHashMap @(SubForm qs) hm

instance KnownSymbol n => FromHashMap (Field n Text) where
  fromHashMap hm = case HM.lookup k hm of
    Nothing -> fail $ "Missing form key: " ++ n
    Just (String t) -> pure $ FieldRaw t
    Just v -> typeMismatch "String or Null" v
    where k = T.pack n
          n = symbolVal (Proxy @n)


instance FromHashMap q => FromJSON (FormRaw q) where
  parseJSON = withObject "FormRaw" fromHashMap

--------------------------------------------------------------------------------

-- Test

data FN = FN Text
data NotEmpty = NotEmpty
data LN = LN Text
data Name = Name FN LN

type NameForm = Unvalidated Name (SubForm
  [ Validated NotEmpty FN (Field "fn" Text)
  , Unvalidated LN (Field "ln" Text)
  ])

nameRaw :: FormRaw NameForm
nameRaw = UnvalidatedRaw $ FormRaw $
  ValidatedRaw (FieldRaw "fn") :| UnvalidatedRaw (FieldRaw "ln") :| Nil

--nameRaw' :: ToRaw NameForm
--nameRaw' = "test" :| "ln" :| Nil

nameErr :: FormErr NameForm
nameErr = UnvalidatedErr $ FormErr $
  ValidatedErr NotEmpty FieldErr :| UnvalidatedErr FieldErr :| Nil

nameHs :: FormHs NameForm
nameHs = UnvalidatedHs $ Name (FN "fn") (LN "ln")

nameSubHs :: FormHs (SubForm [ Validated NotEmpty FN (Field "fn" Text)
                             , Unvalidated LN (Field "ln" Text)] )
nameSubHs = FormHs $ ValidatedHs (FN "fn") :| UnvalidatedHs (LN "ln") :| Nil

