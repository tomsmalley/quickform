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
  , (:+:)

  -- ** Field types
  , FieldType
  , InputField
  , EnumField
  , EnumError (..)

  -- * Term level

  -- ** Form types
  , Form (Form, unForm)
  , reform
  , Touched (..)

  -- ** Reduced types
  , Reduced
  , Raw
  , Hs
  , Err

  -- ** Constructors
  , (:*:) ((:+:))

  -- * Validation

  , anyErrors
  , AnyErrors

  -- ** Validation class
  , Validation (..)
  , ValidationType

  -- ** Full validation
  , validateAll
  , ValidateAll
  , ValidateAllType

  -- ** Validation by branch
  , validateBranch

  -- * Lenses
  , subform
  , (??~)

  ) where

import QuickForm.BranchValidation
import QuickForm.Form
import QuickForm.Lens
import QuickForm.TypeLevel
import QuickForm.Validation
