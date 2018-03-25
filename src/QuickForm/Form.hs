{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

-- | Form wrapper
module QuickForm.Form where

import Data.Aeson
import Data.Aeson.Types
import Data.Default
import Data.HashMap.Strict (HashMap)
import Data.Proxy
import Data.Text (Text)
import Data.Semigroup
import GHC.TypeLits (KnownSymbol, symbolVal, Symbol)

import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

import QuickForm.TypeLevel

-- Type functions --------------------------------------------------------------

-- | Encoding of the form input data
-- This should have a nice JSON encoding
newtype Form q = Form { unForm :: ToForm q }

deriving instance Eq (ToForm q) => Eq (Form q)
deriving instance Show (ToForm q) => Show (Form q)
deriving instance Semigroup (ToForm q) => Semigroup (Form q)
deriving instance Monoid (ToForm q) => Monoid (Form q)
deriving instance Default (ToForm q) => Default (Form q)

type family ToForm (q :: QuickForm) where
  ToForm (Validated e a q) = ToForm q
  ToForm (Field n a) = a
  ToForm (Fields qs) = TList (FormList qs)
type family FormList (q :: [QuickForm]) where
  FormList '[] = '[]
  FormList (q ': qs) = ToForm q ': FormList qs

-- | Encoding of the form output data
type family FormOutput (q :: QuickForm) where
  FormOutput (Validated e a q) = a
  FormOutput (Field n a) = a
  FormOutput (Fields qs) = TList (FormOutputList qs)
type family FormOutputList (q :: [QuickForm]) where
  FormOutputList '[] = '[]
  FormOutputList (q ': qs) = FormOutput q ': FormOutputList qs

type Lens' s a = Lens s s a a
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
-- type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

class FormLens e (form :: QuickForm) (sub :: QuickForm) where
  formLens :: Proxy sub -> Lens' (FormError form) (Maybe e)
instance FormLens e (Validated e a q) (Validated e a q) where
  formLens Proxy f (ValidatedError e q) = (\e' -> ValidatedError e' q) <$> f e
instance FormLens e q sub => FormLens e (Validated e1 a q) sub where
  formLens Proxy f (ValidatedError e q) = ValidatedError e <$> formLens @e @q @sub Proxy f q

-- | Encoding of the form errors
data FormError (q :: QuickForm) where
  ValidatedError :: Maybe e -> FormError q -> FormError (Validated e a q)
  FieldError :: FormError (Field n a)
  FieldsError :: TList (Map FormError qs) -> FormError (Fields qs)

instance (Show e, Show (FormError q)) => Show (FormError (Validated e a q)) where
  show (ValidatedError e q) = unwords ["ValidatedError", show e, show q]
instance Show (TList (Map FormError qs)) => Show (FormError (Fields qs)) where
  show (FieldsError l) = unwords ["FormError", show l]
instance Show (FormError (Field n a)) where
  show FieldError = "FieldError"

instance (Eq e, Eq (FormError q)) => Eq (FormError (Validated e a q)) where
  ValidatedError e q == ValidatedError e' q' = e == e' && q == q'
instance Eq (TList (Map FormError qs)) => Eq (FormError (Fields qs)) where
  FieldsError l == FieldsError l' = l == l'
instance Eq (FormError (Field n a)) where
  FieldError == FieldError = True

instance (Semigroup e, Semigroup (FormError q)) => Semigroup (FormError (Validated e a q)) where
  ValidatedError e q <> ValidatedError e' q' = ValidatedError (e <> e') (q <> q')
instance Semigroup (FormError (Field a q)) where
  FieldError <> FieldError = FieldError
instance Semigroup (TList (Map FormError qs)) => Semigroup (FormError (Fields qs)) where
  FieldsError l <> FieldsError l' = FieldsError (l <> l')

instance (Semigroup (FormError q), Default (FormError q)) => Monoid (FormError q) where
  mempty = def
  mappend = (<>)

instance (Default (FormError q))
  => Default (FormError (Validated e a q)) where
    def = ValidatedError Nothing def
instance Default (TList (Map FormError qs))
  => Default (FormError (Fields qs)) where
    def = FieldsError def
instance Default (FormError (Field n a)) where
  def = FieldError

instance (ToJSON e, ToJSON (FormError q)) => ToJSON (FormError (Validated e a q)) where
  toJSON (ValidatedError e q) = Object $ HM.fromList [("error", toJSON e), ("subform", toJSON q)]


instance ToJSON a => ToJSON (FormError (Field n a)) where
  toJSON FieldError = Null

instance ToValueList (Map FormError qs) => ToJSON (FormError (Fields qs)) where
  toJSON (FieldsError l) = Array $ V.fromList $ toValueList l

class ToValueList l where
  toValueList :: TList l -> [Value]
instance ToValueList '[] where
  toValueList Nil = []
instance (ToJSON a, ToValueList as) => ToValueList (a ': as) where
  toValueList (a :| as) = toJSON a : toValueList as

class FromValueList l where
  fromValueList :: [Value] -> Parser (TList l)
instance FromValueList '[] where
  fromValueList [] = pure Nil
  fromValueList _ = fail "FromValueList: too many items"
instance (FromJSON a, FromValueList as) => FromValueList (a ': as) where
  fromValueList [] = fail "FromValueList: not enough items"
  fromValueList (a : as) = (:|) <$> parseJSON a <*> fromValueList as

instance (FromJSON e, FromJSON (FormError q)) => FromJSON (FormError (Validated e a q)) where
  parseJSON = withObject "ValidatedError" $ \o -> ValidatedError <$> o .: "error" <*> o .: "subform"


instance FromJSON a => FromJSON (FormError (Field n a)) where
  parseJSON _ = pure FieldError

instance FromValueList (Map FormError qs) => FromJSON (FormError (Fields qs)) where
  parseJSON = withArray "FieldsError" $ fmap FieldsError . fromValueList . V.toList

--------------------------------------------------------------------------------

class ToHashMap q where
  toHashMap :: Form q -> HashMap Text Value

instance ToHashMap q => ToHashMap (Validated e a q) where
  toHashMap (Form q) = toHashMap (Form @q q)

instance (ToJSON a, KnownSymbol n)
  => ToHashMap (Field n a) where
  toHashMap (Form value) = HM.singleton name $ toJSON value
    where name = T.pack $ symbolVal $ Proxy @n

instance ToHashMap (Fields '[]) where
  toHashMap (Form Nil) = HM.empty

instance (ToHashMap q, ToHashMap (Fields qs))
  => ToHashMap (Fields (q ': qs)) where
    toHashMap (Form (q :| qs))
      = toHashMap (Form @q q) <> toHashMap @(Fields qs) (Form qs)

instance ToHashMap q => ToJSON (Form q) where
  toJSON q = Object $ toHashMap q

--------------------------------------------------------------------------------

class FromHashMap q where
  fromHashMap :: HashMap Text Value -> Parser (Form q)

instance FromHashMap q => FromHashMap (Validated e a q) where
  fromHashMap = fmap (Form . unForm @q) . fromHashMap

instance FromHashMap (Fields '[]) where
  fromHashMap _ = pure $ Form Nil

instance (FromHashMap q, FromHashMap (Fields qs))
  => FromHashMap (Fields (q ': qs)) where
  fromHashMap hm = (\(Form r) (Form rs) -> Form $ r :| rs)
    <$> fromHashMap @q hm <*> fromHashMap @(Fields qs) hm

instance (FromJSON a, KnownSymbol n)
  => FromHashMap (Field n a) where
  fromHashMap hm = case HM.lookup k hm of
    Nothing -> fail $ "Missing form key: " ++ n
    Just v -> Form <$> parseJSON v
    where k = T.pack n
          n = symbolVal (Proxy @n)

instance FromHashMap q => FromJSON (Form q) where
  parseJSON = withObject "Form" fromHashMap

