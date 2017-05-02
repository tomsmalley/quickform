{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
--{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module React.Flux.Forms.Actions where

import Control.DeepSeq (NFData(..))
import Data.Text (Text)
import Data.Proxy (Proxy(..))
import GHC.Generics (Generic)
import Data.Aeson

import QuickForm

-- Different module for dispatcher
data FormAction a
  = Submit
  | SetFormError (Form Err a)
  | UpdateState (Form Raw a -> Form Raw a) (Form Raw a -> Form Err a)
  deriving (Generic)

deriving instance (NFData (Form Err a)) => NFData (FormAction a)
