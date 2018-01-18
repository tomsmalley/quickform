{-# LANGUAGE DeriveFunctor, DeriveGeneric, TypeFamilies
           , UndecidableInstances #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

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
instance EmptySetErrors () where
  emptySetErrors = ()
instance Monoid a => EmptySetErrors (Touched a) where
  emptySetErrors = Touched mempty
instance (EmptySetErrors a, EmptySetErrors b) => EmptySetErrors (a, b) where
  emptySetErrors = (emptySetErrors, emptySetErrors)
--instance (EmptySetErrors a, EmptySetErrors b) => EmptySetErrors (a :*: b) where
--  emptySetErrors = emptySetErrors :+: emptySetErrors

instance (EmptySetErrors (Form Err sub), Ord e)
  => EmptySetErrors (Form Err (Validated e a sub)) where
  emptySetErrors = Validated emptySetErrors emptySetErrors

instance (EmptySetErrors (Form Err sub))
  => EmptySetErrors (Form Err (Unvalidated a sub)) where
  emptySetErrors = Unvalidated emptySetErrors emptySetErrors

instance KnownSymbol n => EmptySetErrors (Form Err (Field n InputField)) where
  emptySetErrors = InputField emptySetErrors

instance (EmptySetErrors (Form Err f1), EmptySetErrors (Form Err f2))
  => EmptySetErrors (Form Err (f1 :+: f2)) where
    emptySetErrors = Pair emptySetErrors emptySetErrors emptySetErrors

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

-- | Stores the raw form values (e.g. Text from an <input> element)
data Raw
-- | Stores the validated haskell types
data Err
-- | Stores errors from validation
data Hs

type family Args r (f :: QuickForm)

data Form r (f :: QuickForm) where
  Validated
    :: EmptySetErrors (Form Err f)
    => Args r (Validated e a f)
    -> Form r f
    -> Form r (Validated e a f)
  Unvalidated
    :: Args r (Unvalidated a f)
    -> Form r f
    -> Form r (Unvalidated a f)
  InputField
    :: KnownSymbol n
    => Args r (Field n InputField)
    -> Form r (Field n InputField)
  Pair
    :: (EmptySetErrors (Form Err f), EmptySetErrors (Form Err g))
    => Args r (f :+: g)
    -> Form r f
    -> Form r g
    -> Form r (f :+: g)

symbolText :: KnownSymbol s => Proxy s -> Text
symbolText = T.pack . symbolVal

rawHashMap :: Form Raw f -> HashMap Text Value
rawHashMap (Validated _ f) = rawHashMap f
rawHashMap (Unvalidated _ f) = rawHashMap f
rawHashMap (Pair _ f g) = rawHashMap f <> rawHashMap g
rawHashMap (InputField (n, t))
  = HM.singleton (symbolText n) $ touched Null String t

class FromHashMap f where
  fromHashMap :: HashMap Text Value -> Parser (Form Raw f)

instance (EmptySetErrors (Form Err f), FromHashMap f)
  => FromHashMap (Validated e a f) where
    fromHashMap = fmap (Validated ()) . fromHashMap

instance FromHashMap f => FromHashMap (Unvalidated a f) where
  fromHashMap = fmap (Unvalidated ()) . fromHashMap

instance
  ( FromHashMap f, FromHashMap g
  , EmptySetErrors (Form Err f), EmptySetErrors (Form Err g)
  ) => FromHashMap (f :+: g) where
  fromHashMap hm = Pair () <$> fromHashMap hm <*> fromHashMap hm

instance KnownSymbol n => FromHashMap (Field n InputField) where
  fromHashMap hm = case HM.lookup k hm of
    Nothing -> fail $ "Missing form key: " ++ n
    Just (String t) -> pure $ InputField (Proxy, Touched t)
    Just Null -> pure $ InputField (Proxy, Untouched)
    Just v -> typeMismatch "String or Null" v
    where k = T.pack n
          n = symbolVal (Proxy @n)

instance ToJSON (Form Raw f) where
  toJSON f = Object $ rawHashMap f

instance FromHashMap f => FromJSON (Form Raw f) where
  parseJSON = withObject "Form Raw" fromHashMap

type instance Args Err (Validated e _ _) = Touched (Set e)
type instance Args Err (Unvalidated _ _) = ()
type instance Args Err (Field _ InputField) = ()
type instance Args Err (a :+: b) = ()

type instance Args Raw (Validated _ _ _) = ()
type instance Args Raw (Unvalidated _ _) = ()
type instance Args Raw (Field n InputField) = (Proxy n, Touched Text)
type instance Args Raw (a :+: b) = ()

type instance Args Hs (Validated _ a _) = a
type instance Args Hs (Unvalidated a _) = a
type instance Args Hs (Field _ InputField) = Text
type instance Args Hs (a :+: b) = ()

instance (Eq (Form r f), Eq (Args r (Validated e a f)))
  => Eq (Form r (Validated e a f)) where
  Validated a b == Validated a' b' = a == a' && b == b'

instance (Eq (Form r f), Eq (Args r (Unvalidated a f)))
  => Eq (Form r (Unvalidated a f)) where
  Unvalidated a b == Unvalidated a' b' = a == a' && b == b'

instance (Eq (Form r f), Eq (Form r g), Eq (Args r (f :+: g)))
  => Eq (Form r (f :+: g)) where
    Pair x a b == Pair x' a' b' = x == x' && a == a' && b == b'

instance (Eq (Args r (Field n InputField)))
  => Eq (Form r (Field n InputField)) where
  InputField a == InputField a' = a == a'

instance (Show (Form r sub), Show (Args r (Validated e a sub)))
  => Show (Form r (Validated e a sub)) where
    show (Validated a b) = "Validated " ++ show a ++ " " ++ show b

instance (Show (Form r sub), Show (Args r (Unvalidated a sub)))
  => Show (Form r (Unvalidated a sub)) where
    show (Unvalidated a b) = "Unvalidated " ++ show a ++ " " ++ show b

instance (Show (Form r f1), Show (Form r f2), Show (Args r (f1 :+: f2)))
  => Show (Form r (f1 :+: f2)) where
    show (Pair args a b) = "Pair " ++ show args ++ " " ++ show a ++ " " ++ show b
instance (Show (Args r (Field n InputField)))
  => Show (Form r (Field n InputField)) where
    show (InputField args) = "InputField " ++ show args

