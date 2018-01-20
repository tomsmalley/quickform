{-# LANGUAGE PatternSynonyms #-}

-- | QuickForm is a library for building type level descriptions of HTML forms,
-- providing data level representations for raw form data, field errors,
-- and final Haskell types in a type safe manner. It is designed to be used by
-- web services written entirely in Haskell, that is, on both the back end and
-- the front end (via GHCJS and accompanying QuickForm front end library).
--
-- Features include:
--
-- * Modular form definitions: stack sub forms together to form larger forms
--
-- * Validation of raw form data into your specified Haskell or error types
--
--      * Full form validation for both client and server
--      * Branch-wise validation (only validating the chosen field or sub form)
--      for validation as you type
--
-- * Automatic aeson conversion for server-client communication
--
--      * Raw data is automatically converted
--      * Just provide FromJSON and ToJSON instances for your error types
--      * Don't need to provide instances for Haskell data, it shouldn't need to
--      be sent over the network
--
module QuickForm
  (

  -- * Type level

  -- ** QuickForm kind and type constructors
    QuickForm
  , Validated
  , Unvalidated
  , Field
  , SubForm

  -- * Term level

  -- ** Form types
  , Touched (..)
  , hasError

  , FormErr (..)
  , FormRaw (..)
  , FormHs (..)
  , HList (..)
  , GetHs (..)

  , module QuickForm.Validation

  -- ** Constructors

  -- * Validation

--  , anyErrors
--  , AnyErrors
--  , hasError

  ) where

import QuickForm.Form
import QuickForm.TypeLevel
import QuickForm.Validation
