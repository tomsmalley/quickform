{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module React.Flux.Forms where

import Control.Lens
import Data.Proxy
import Data.Maybe (fromMaybe)
import GHC.TypeLits
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (intersperse, intercalate)

import QuickForm
import QuickForm.Lens
import QuickForm.TypeLevel
import QuickForm.Validation
import QuickForm.BranchValidation

import React.Flux (someStoreAction, br_, SomeStoreAction, Event, CallbackFunction, PropertyOrHandler , ReactElementM, onSubmit, preventDefault, elemString, classNames)
import React.Flux.Forms.Store
import React.Flux.Forms.Actions

import qualified React.Flux.SemanticUI as SUI
import React.Flux.SemanticUI ((@=), ($=), (&=), (.:))

import GHCJS.Marshal (FromJSVal (..), ToJSVal (..))
import Data.Typeable (Typeable (..))
import Control.DeepSeq (NFData (..))

import Control.Monad.Reader

type FormReader f = ReaderT (FormStore f) (ReactElementM [SomeStoreAction]) ()

controlledForm_ :: forall f. FormConstraints f
                 => [PropertyOrHandler [SomeStoreAction]]
                 -> FormReader f
                 -> FormReader f
controlledForm_ props children =
  SUI.form_ (onSubmit f : props) `mapReaderT` children
  where
    f event = preventDefault event `seq` [someStoreAction @(FormStore f) Submit]

class PutForm (field :: QuickForm) (form :: QuickForm) where
  putForm :: FormReader form

class FormProps (field :: QuickForm) (form :: QuickForm) where
  props :: [PropertyOrHandler [SomeStoreAction]]
  props = []
  errProps :: [PropertyOrHandler [SomeStoreAction]]
  errProps = []
  label :: Label
  label = NoLabel

instance {-# OVERLAPPABLE #-} FormProps a b where

-- Validated parent
instance {-# OVERLAPPABLE #-} forall e a b form field.
  ( PutForm b form, FormProps field form
  , PutError e
  , field ~ (Validated e a b)
  , FormLens field form Err
  , HasError b ~ 'False -- TODO need to case for true
  ) => PutForm (Validated e a b) form where
    putForm = do
      -- Field label
      lift $ putLabel $ label @field @form
      state <- ask
      let err = state ^. errors . subform @field :: Checked (Set e)
      -- Wrap sub fields in a group
      SUI.formGroup_ (classNames [("error", fieldHasError err)] : ("widths" $= "equal") : props @field @form)
        `mapReaderT` do
        putForm @b
        -- Put the possible validation error message
        lift $ SUI.message_ (("error" &= True) : errProps @field @form)
          . sequence_ . intersperse (br_ []) . map putError
          $ S.toAscList $ fromChecked S.empty err

fromChecked :: a -> Checked a -> a
fromChecked _ (Checked a) = a
fromChecked a Unchecked = a

{-
-- Concrete validated text input field
instance {-# OVERLAPPABLE #-} forall n e o form field.
  ( KnownSymbol n, FormConstraints form, PutError e
  , FormProps field form, FieldLens field form 'Raw, FieldLens field form 'Err
  , ValidateBranch field form
  , field ~ (NamedFieldSetV n e o :<: TextInput)
  ) => PutForm (NamedFieldSetV n e o :<: TextInput) form where
    putForm = do
      state <- ask
      let err = fieldHasError $ state ^. errors . field @field . firstField
      -- Put the form field in the semantic wrapper with error state
      lift $ SUI.formField_ [ "error" &= err ] $ do
        -- Field label
        putLabel (label @field @form)
        -- Actual input element
        SUI.input_ $ (props @field @form) ++
          [ "name" &= symbolVal (Proxy @n)
          , "value" &= state ^. raw . field @field
          , SUI.onChangeSUI $ \ e d -> pure $
            someStoreAction @(FormStore form) $
              UpdateState (field @field .~ d .: "value") (validateBranch @field)
          ]
        -- Small pointing error message. Because this should be small, we only
        -- show the most important (i.e. first element in ascending list) error
        -- of the given set.
        case S.minView =<< state ^. errors . field @field . firstField of
          Nothing -> mempty
          Just (err, _) -> SUI.label_ (("pointing" $= "above")
                          : errProps @field @form) $ putError err
-}

fieldHasError :: Checked (Set a) -> Bool
fieldHasError Unchecked = False
fieldHasError (Checked s) = not $ S.null s


-- Unvalidated parent
instance {-# OVERLAPPABLE #-} forall a b form field.
  ( PutForm b form, FormProps field form
  , field ~ (Unvalidated a b)
  ) => PutForm (Unvalidated a b) form where
    putForm = do
      lift $ putLabel $ label @field @form
      SUI.formGroup_ (props @field @form)
        `mapReaderT` putForm @b

-- Concrete text input field
instance {-# OVERLAPPABLE #-} forall n form field.
  ( KnownSymbol n, FormProps field form
  , FormConstraints form
  , ValidateBranch field form
  , FormLens field form Raw
  , field ~ (Field n (InputField TextInput))
  ) => PutForm (Field n (InputField TextInput)) form where
  putForm = do
    state <- ask
    lift $ SUI.formField_ [] $ do
      putLabel (label @field @form)
      SUI.input_ $
        [ "name" &= symbolVal (Proxy :: Proxy n)
        , "value" &= state ^. raw . subform @field @form
        , "type" $= "text"
        , SUI.onChangeSUI $ \ e d -> pure $ someStoreAction @(FormStore form) $
          let val = d .: "value" in
            UpdateState (subform @field .~ val) (validateBranch @field)
        ] ++ (props @field @form)

-- Concrete email input field
instance {-# OVERLAPPABLE #-} forall n form field.
  ( KnownSymbol n, FormProps field form
  , FormConstraints form
  , ValidateBranch field form
  , FormLens field form Raw
  , field ~ (Field n (InputField EmailInput))
  ) => PutForm (Field n (InputField EmailInput)) form where
  putForm = do
    state <- ask
    lift $ SUI.formField_ [] $ do
      putLabel (label @field @form)
      SUI.input_ $
        [ "name" &= symbolVal (Proxy :: Proxy n)
        , "value" &= state ^. raw . subform @field @form
        , "type" $= "email"
        , SUI.onChangeSUI $ \ e d -> pure $ someStoreAction @(FormStore form) $
          let val = d .: "value" in
            UpdateState (subform @field .~ val) (validateBranch @field)
        ] ++ (props @field @form)

-- Concrete password input field
instance {-# OVERLAPPABLE #-} forall n form field.
  ( KnownSymbol n, FormProps field form
  , FormConstraints form
  , ValidateBranch field form
  , FormLens field form Raw
  , field ~ (Field n (InputField PasswordInput))
  ) => PutForm (Field n (InputField PasswordInput)) form where
  putForm = do
    state <- ask
    lift $ SUI.formField_ [] $ do
      putLabel (label @field @form)
      SUI.input_ $
        [ "name" &= symbolVal (Proxy :: Proxy n)
        , "value" &= state ^. raw . subform @field @form
        , "type" $= "password"
        , SUI.onChangeSUI $ \ e d -> pure $ someStoreAction @(FormStore form) $
          let val = d .: "value" in
            UpdateState (subform @field .~ val) (validateBranch @field)
        ] ++ (props @field @form)

-- Concrete hidden input field
instance {-# OVERLAPPABLE #-} forall n form field.
  ( KnownSymbol n, FormProps field form
  , FormConstraints form
  , ValidateBranch field form
  , FormLens field form Raw
  , field ~ (Field n (InputField HiddenInput))
  ) => PutForm (Field n (InputField HiddenInput)) form where
  putForm = do
    state <- ask
    lift $ SUI.input_ $
      [ "name" &= symbolVal (Proxy :: Proxy n)
      , "value" &= state ^. raw . subform @field @form
      , "type" $= "hidden"
      , SUI.onChangeSUI $ \ e d -> pure $ someStoreAction @(FormStore form) $
        let val = d .: "value" in
          UpdateState (subform @field .~ val) (validateBranch @field)
      ] ++ (props @field @form)

-- Pair
instance forall a b form. (PutForm a form, PutForm b form)
  => PutForm (a :+: b) form where
    putForm = do
      putForm @a @form
      putForm @b @form

data Label
  = Label (ReactElementM [SomeStoreAction] ())
  | NoLabel

putLabel :: Label -> ReactElementM [SomeStoreAction] ()
putLabel (Label l) = l
putLabel _ = mempty

data ErrorLabel
  = ErrorLabel Position (ReactElementM [SomeStoreAction] ())
  | NoErrorLabel

data Position = Before | After

class PutError err where
  putError :: err -> ReactElementM [SomeStoreAction] ()

{-
-- Choose the "largest" error and display it
errorLabel :: PutError err
        => Position
        -> [PropertyOrHandler [SomeStoreAction]]
        -> Set err
        -> ErrorLabel
errorLabel position props s
  = ErrorLabel position $
      case S.maxView s of
        Just (err, _) -> SUI.label_ props $ putError err
        Nothing       -> SUI.label_ ["className" $= "hidden"] mempty
                      -- Changing / removing element causes loss of focus,
                      -- so we leave it in but hide it.

errorMessage_ :: forall f s proxy err hs.
  ( FormConstraints f
  , FieldLens s (FormEither f) (Validation err hs) (Validation err hs)
  , PutError err
  ) => proxy s -> FormReader f
errorMessage_ p = do
  state <- ask
  case _errors state ^. fieldLens p of
    Invalid errset -> lift $ SUI.message_ ["error" &= True] $
      sequence_ $ intersperse (br_ []) $ map putError $ S.toAscList errset
    _ -> lift $ mempty
-}

noErrorLabel :: Set err -> ErrorLabel
noErrorLabel = const NoErrorLabel

putErrorBefore :: ErrorLabel -> ReactElementM [SomeStoreAction] ()
putErrorBefore (ErrorLabel Before e) = e
putErrorBefore _ = mempty

putErrorAfter :: ErrorLabel -> ReactElementM [SomeStoreAction] ()
putErrorAfter (ErrorLabel After e) = e
putErrorAfter _ = mempty

hasError :: Set a -> Bool
hasError = not . S.null

isInvalid :: Either (Set a) b -> Bool
isInvalid (Left s) = not $ S.null s
isInvalid _ = False

submitButton_ :: forall f. ReactElementM [SomeStoreAction] () -> FormReader f
submitButton_ child = do
  state <- ask
  lift $ SUI.submitButton_ ["loading" &= _pending state] child

{-
unvalidatedInput_ :: forall f s proxy hs.
  ( FormConstraints f, KnownSymbol s
  , FieldLens s (FormRaw f) Text Text
  , FieldLens s (FormEither f) hs hs
  , Unvalidated Text hs
  , CallbackFunction [SomeStoreAction] [SomeStoreAction]
  ) => proxy s
    -> ReactElementM [SomeStoreAction] ()
    -> [PropertyOrHandler [SomeStoreAction]]
    -> FormReader f
unvalidatedInput_ p label props = do
  state <- ask
  lift $ SUI.formField_ [] $ do
      label
      SUI.input_ $ props ++
        [ "name" &= symbolVal p
        , "value" &= _raw state ^. fieldLens p
        , SUI.onChangeSUI $ \ e d -> pure $ someStoreAction @(FormStore f) $
            UpdateState (fieldLens p .~ d .: "value") id
        ]
-}
