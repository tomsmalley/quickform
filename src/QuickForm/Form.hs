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
import GHC.TypeLits

import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import qualified Data.Set as S

import QuickForm.TypeLevel

--  Form type -------------------------------------------------------------------

hasError :: Touched (Set e) -> Bool
hasError Untouched = False
hasError (Touched s) = not $ S.null s

-- | Like monoid's mempty, but produces Just mempty instead of Nothing
class EmptySetErrors a where
  emptySetErrors :: a
instance EmptySetErrors [a] where
  emptySetErrors = []
instance EmptySetErrors () where
  emptySetErrors = ()
instance Monoid a => EmptySetErrors (Touched a) where
  emptySetErrors = Touched mempty
instance (EmptySetErrors a, EmptySetErrors b) => EmptySetErrors (a, b) where
  emptySetErrors = (emptySetErrors, emptySetErrors)
instance EmptySetErrors (HList '[]) where
  emptySetErrors = HNil
instance (EmptySetErrors a, EmptySetErrors (HList as))
  => EmptySetErrors (HList (a ': as)) where
  emptySetErrors = emptySetErrors :| emptySetErrors

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

data FormRaw (q :: QuickForm) where
  ValidatedRaw :: FormRaw q -> FormRaw (Validated e a q)
  UnvalidatedRaw :: FormRaw q -> FormRaw (Unvalidated a q)
  FieldRaw :: Proxy n -> a -> FormRaw (Field n a)
  FormRaw :: HList (Map FormRaw q) -> FormRaw (SubForm q)

instance Eq (FormRaw q) => Eq (FormRaw (Validated e a q)) where
  ValidatedRaw f == ValidatedRaw g = f == g
instance Eq (FormRaw q) => Eq (FormRaw (Unvalidated a q)) where
  UnvalidatedRaw f == UnvalidatedRaw g = f == g
instance Eq a => Eq (FormRaw (Field n a)) where
  FieldRaw Proxy a == FieldRaw Proxy a' = a == a'
instance Eq (HList (Map FormRaw q)) => Eq (FormRaw (SubForm q)) where
  FormRaw f == FormRaw g = f == g

instance Show (FormRaw q) => Show (FormRaw (Validated e a q)) where
  show (ValidatedRaw f) = "ValidatedRaw " ++ show f
instance Show (FormRaw q) => Show (FormRaw (Unvalidated a q)) where
  show (UnvalidatedRaw f) = "UnvalidatedRaw " ++ show f
instance Show a => Show (FormRaw (Field n a)) where
  show (FieldRaw Proxy a) = "FieldRaw Proxy " ++ show a
instance Show (HList (Map FormRaw q)) => Show (FormRaw (SubForm q)) where
  show (FormRaw f) = "FormRaw " ++ show f

data FormHs (q :: QuickForm) where
  ValidatedHs :: a -> FormHs (Validated e a q)
  UnvalidatedHs :: a -> FormHs (Unvalidated a q)
  FieldHs :: a -> FormHs (Field n a)
  FormHs :: HList (Map FormHs q) -> FormHs (SubForm q)

class GetHs q a | q -> a where
  getHs :: FormHs q -> a

instance GetHs (Validated e a q) a where
  getHs (ValidatedHs a) = a
instance GetHs (Unvalidated a q) a where
  getHs (UnvalidatedHs a) = a
instance GetHs (Field n a) a where
  getHs (FieldHs a) = a

data FormErr (q :: QuickForm) where
  ValidatedErr :: e -> FormErr q -> FormErr (Validated e a q)
  UnvalidatedErr :: FormErr q -> FormErr (Unvalidated a q)
  FieldErr :: FormErr (Field n a)
  FormErr :: HList (Map FormErr q) -> FormErr (SubForm q)

instance (EmptySetErrors e, EmptySetErrors (FormErr q), Ord e)
  => EmptySetErrors (FormErr (Validated e a q)) where
  emptySetErrors = ValidatedErr emptySetErrors emptySetErrors
instance EmptySetErrors (FormErr q)
  => EmptySetErrors (FormErr (Unvalidated a q)) where
  emptySetErrors = UnvalidatedErr emptySetErrors
instance KnownSymbol n
  => EmptySetErrors (FormErr (Field n Text)) where
  emptySetErrors = FieldErr
instance (EmptySetErrors (HList (Map FormErr q)))
  => EmptySetErrors (FormErr (SubForm q)) where
    emptySetErrors = FormErr emptySetErrors

{-
class ToHashMap f where
  toHashMap :: FormRaw f -> HashMap Text Value
instance ToHashMap (Validated e a f) where
  toHashMap (ValidatedRaw f) = toHashMap f
instance ToHashMap (Unvalidated a f) where
  toHashMap (UnvalidatedRaw f) = toHashMap f
instance (ToHashMap f, ToHashMap (SubForm fs))
  => ToHashMap (SubForm (f ': fs)) where
  toHashMap (FormRaw (f :| fs)) = toHashMap f <> toHashMap (FormRaw fs)

rawHashMap :: FormRaw f -> HashMap Text Value
rawHashMap (ValidatedRaw f) = rawHashMap f
rawHashMap (UnvalidatedRaw f) = rawHashMap f
rawHashMap (FormRaw HNil) = HM.empty
rawHashMap (FormRaw (f :| fs)) = rawHashMap f <> rawHashMap (FormRaw fs)
rawHashMap (FieldRaw n t)
  = HM.singleton (symbolText n) $ touched Null String t

class FromHashMap f where
  fromHashMap :: HashMap Text Value -> Parser (FormRaw f)

instance FromHashMap f => FromHashMap (Validated e a f) where
  fromHashMap = fmap ValidatedRaw . fromHashMap

instance FromHashMap f => FromHashMap (Unvalidated a f) where
  fromHashMap = fmap UnvalidatedRaw . fromHashMap

instance FromHashMap (SubForm f) where
  fromHashMap hm = FormRaw <$> fromHashMap hm

instance KnownSymbol n => FromHashMap (Field n InputField) where
  fromHashMap hm = case HM.lookup k hm of
    Nothing -> fail $ "Missing form key: " ++ n
    Just (String t) -> pure $ FieldRaw Proxy (Touched t)
    Just Null -> pure $ FieldRaw Proxy Untouched
    Just v -> typeMismatch "String or Null" v
    where k = T.pack n
          n = symbolVal (Proxy @n)

instance ToJSON (FormRaw f) where
  toJSON f = Object $ rawHashMap f

instance FromHashMap f => FromJSON (FormRaw f) where
  parseJSON = withObject "Form Raw" fromHashMap
-}


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
  ValidatedRaw (FieldRaw Proxy "fn")
  :| UnvalidatedRaw (FieldRaw Proxy "ln") :| HNil

--nameRaw' :: ToRaw NameForm
--nameRaw' = "test" :| "ln" :| HNil

nameErr :: FormErr NameForm
nameErr = UnvalidatedErr $ FormErr $
  ValidatedErr NotEmpty FieldErr :| UnvalidatedErr FieldErr :| HNil

nameHs :: FormHs NameForm
nameHs = UnvalidatedHs $ Name (FN "fn") (LN "ln")

nameSubHs :: FormHs (SubForm [ Validated NotEmpty FN (Field "fn" Text)
                             , Unvalidated LN (Field "ln" Text)] )
nameSubHs = FormHs $ ValidatedHs (FN "fn") :| UnvalidatedHs (LN "ln") :| HNil

