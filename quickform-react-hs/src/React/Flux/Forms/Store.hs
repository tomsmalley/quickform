{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ConstraintKinds  #-} -- for FormConstraint constraint synonym
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances  #-}

module React.Flux.Forms.Store where

import Data.Monoid ((<>))
import Control.Lens
import Control.DeepSeq
import Control.Monad (when)
import Data.Default (Default(..))
import Data.Maybe (isJust, isNothing)
import Data.Proxy (Proxy(..))
import Data.Set (Set)
import qualified Data.Set as S
import React.Flux
import Control.Lens
import GHC.Generics
import qualified Data.HashMap.Lazy as M
import Data.Text (Text)
import Data.Aeson
import Data.Typeable (Typeable)

import QuickForm
import QuickForm.Validation
import QuickForm.TypeLevel
import React.Flux.Forms.Actions

data FormStatus
  = Pending -- ^ When we are waiting for a response from an ajax request
  | Waiting -- ^ Idle state
  deriving (Eq, Generic, NFData)

data FormStore a = FormState
  { _pending  :: Bool                 -- ^ Are we waiting for an ajax response?
  , _raw      :: Form Raw a          -- ^ The raw form data (controls components)
  , _errors   :: Form Err a          -- ^ Validation states
  , _onSubmit :: Form Raw a -> IO () -- ^ Submit callback handler
  } deriving (Generic)

instance (Eq (Form Raw a), Eq (Form Err a))
  => Eq (FormStore a) where
    a == b = _pending a == _pending b
          && _raw a == _raw b
          && _errors a == _errors b


errors :: Lens' (FormStore a) (Form Err a)
errors f (FormState p r e s) = (\e' -> FormState p r e' s) <$> f e

raw :: Lens' (FormStore a) (Form Raw a)
raw f (FormState p r e s) = (\r' -> FormState p r' e s) <$> f r

deriving instance
    (NFData (Form Raw a), NFData (Form Err a))
    => NFData (FormStore a)

type FormConstraints a =
  ( Monoid (Form Err a), NFData (Form Raw a), NFData (Form Err a)
  , Show (Form Err a), Show (Form Raw a)
  , Typeable a, ValidateAll a, HasError a ~ 'True)

instance FormConstraints a
      => StoreData (FormStore a) where
  type StoreAction (FormStore a) = FormAction a

  transform Submit s = do
    case validateAll $ _raw s of
      Left err -> return s { _errors = err }
      Right _  -> do
        _onSubmit s (_raw s)
        return $ s { _errors = mempty, _pending = True }

  transform (SetFormError formErr) s =
    return $ s { _pending = False, _errors = formErr <> _errors s }

  transform (UpdateState f g) s = do
    let raw = f $ _raw s
        errors = _errors s <> g raw
    print raw
    print $ g raw
    print $ _errors s
    print errors
    return $ force $ s { _raw = raw, _errors = errors }

