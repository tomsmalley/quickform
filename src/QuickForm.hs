{-# LANGUAGE PatternSynonyms #-}

-- | QuickForm is a library for building type level descriptions of HTML forms,
-- providing data level representations for raw form data, field errors,
-- and final Haskell types. It is intended to be used by web services written
-- entirely in Haskell, that is, on both the backend and the frontend (via
-- GHCJS and accompanying QuickForm frontend library).
--
-- Features include:
--
-- * Modular form definitions: stack sub forms together to construct larger forms
-- * Validation of raw form data into your specified Haskell or error types
--
module QuickForm
  ( module QuickForm.Form
  , module QuickForm.TypeLevel
  , module QuickForm.Validation
  ) where

import QuickForm.Form
import QuickForm.TypeLevel
import QuickForm.Validation
