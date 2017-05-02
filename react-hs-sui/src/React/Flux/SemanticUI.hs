{-# LANGUAGE OverloadedStrings #-}

module React.Flux.SemanticUI
  ( module React.Flux
  -- * React.Flux custom fixity
  , (&=)
  , (@=)
  , ($=)
  -- * Forms
  , authForm_
  -- * Fields
  , emailField_
  , passwordField_
  -- * Other form elements
  , submitButton_
  -- * Map elements
  , map_
  , tileLayer_
  , marker_
  , polyline_

  -- * Semantic React
  , dropdown_
  , dropdownMenu_
  , dropdownDivider_
  , dropdownItem_

  , menu_
  , menuItem_
  , menuMenu_

  , form_
  , formField_
  , formGroup_
  , segment_
  , button_
  , input_
  , label_
  , radio_
  , checkbox_
  , message_
  , onChangeSUI

  , (.:)
  ) where

import Data.Aeson (ToJSON)
import Data.JSString (JSString)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import System.IO.Unsafe (unsafePerformIO)

import React.Flux.Internal (el)
import Data.JSString (JSString)
import GHCJS.Marshal (ToJSVal, FromJSVal(..))
import GHCJS.Prim (JSVal)
import qualified React.Flux (($=), (&=), (@=))
import React.Flux (ReactElementM, PropertyOrHandler, CallbackFunction
                  , callback, foreignClass)

(@=) :: ToJSON a => JSString -> a -> PropertyOrHandler handler
(@=) = (React.Flux.@=)
infixr 0 @=

(&=) :: ToJSVal a => JSString -> a -> PropertyOrHandler handler
(&=) = (React.Flux.&=)
infixr 0 &=

($=) :: JSString -> JSString -> PropertyOrHandler handler
($=) = (React.Flux.$=)
infixr 0 $=

-- Semantic UI Combinators -----------------------------------------------------

-- | Email input field
emailField_ :: Bool
            -> [PropertyOrHandler h]  -- ^ Properties for input_ element
            -> ReactElementM h ()
emailField_ err props = formField_ ["error" &= err] $ input_ $ props ++
  [ "type" $= "email", "placeholder" $= "E-mail address"
  , "icon" $= "user", "iconPosition" $= "left" ]

-- | Password input field
passwordField_ :: Bool
               -> [PropertyOrHandler h]  -- ^ Properties for input_ element
               -> ReactElementM h ()
passwordField_ err props = formField_ ["error" &= err] $ input_ $ props ++
  [ "type" $= "password", "placeholder" $= "Password"
  , "icon" $= "lock", "iconPosition" $= "left" ]

-- | Large submit button
submitButton_ :: [PropertyOrHandler h]
              -> ReactElementM h () -- ^ Child element
              -> ReactElementM h ()
submitButton_ props = button_ $ props ++
  [ "type" $= "submit", "primary" &= True, "size" $= "large", "fluid" &= True ]

-- | Main auth form
authForm_ :: [PropertyOrHandler h]  -- ^ Properties
          -> ReactElementM h ()     -- ^ Child element
          -> ReactElementM h ()
authForm_ props = form_ $ props ++
  [ "id" $= "auth-form", "size" $= "large", "className" $= "column" ]

-- Map elements ----------------------------------------------------------------

map_ :: (Double, Double)            -- ^ Latitude, longitude map center
     -> Int                         -- ^ Initial zoom level
     -> [PropertyOrHandler h] -- ^ Properties
     -> ReactElementM h ()    -- ^ Child elements (minimum: tileLayer_)
     -> ReactElementM h ()
map_ center zoom props = foreignClass (js_reactLeaflet "Map") $ props ++
  [ "id" $= "map", "center" @= center, "zoom" @= zoom ]

-- | OSM Tile layer for leaflet
tileLayer_ :: ReactElementM h ()
tileLayer_ = reactLeaflet "TileLayer"
  [ "url" $= "http://{s}.tile.osm.org/{z}/{x}/{y}.png"
  , "attribution" $= "&copy; <a href=\"http://osm.org/copyright\">" <>
    "OpenStreetMap</a> contributors" ] mempty

marker_ :: (Double, Double) -> ReactElementM h ()
        -> ReactElementM h ()
marker_ p = reactLeaflet "Marker" [ "position" @= p ]

polyline_ :: [(Double, Double)] -> ReactElementM h ()
polyline_ ps = reactLeaflet "Polyline" [ "positions" @= ps ] mempty

-- | Helper function for making ReactLeaflet elements into ReactElementM
reactLeaflet :: JSString                          -- ^ Element name
             -> [PropertyOrHandler h]  -- ^ Properties
             -> ReactElementM h a      -- ^ Child elements
             -> ReactElementM h a
reactLeaflet = foreignClass . js_reactLeaflet

foreign import javascript unsafe
  "$r = window.ReactLeaflet[$1]"
  js_reactLeaflet :: JSString -> JSVal


-- Semantic UI Base ------------------------------------------------------------

-- | Helper function for making SemanticUI elements into ReactElementM
semanticUI :: JSString              -- ^ Element name
           -> [PropertyOrHandler h] -- ^ Properties
           -> ReactElementM h a     -- ^ Child elements
           -> ReactElementM h a
semanticUI = foreignClass . js_semanticUIReact

foreign import javascript unsafe
  "$r = window.semanticUIReact[$1]"
  js_semanticUIReact :: JSString -> JSVal

dropdown_ :: Text                             -- ^ Text
          -> [PropertyOrHandler h] -- ^ Properties
          -> ReactElementM h a     -- ^ Child elements
          -> ReactElementM h a
dropdown_ text props = semanticUI "Dropdown" $ props ++ ["text" &= text]

dropdownMenu_ :: ReactElementM h a -> ReactElementM h a
dropdownMenu_ = semanticUI "DropdownMenu" []

dropdownDivider_ :: ReactElementM h ()
dropdownDivider_ = semanticUI "DropdownDivider" [] mempty

dropdownItem_ :: Text
              -> [PropertyOrHandler h]
              -> ReactElementM h a
              -> ReactElementM h a
dropdownItem_ text props = semanticUI "DropdownItem" $ props ++ ["text" &= text]

menu_ :: [PropertyOrHandler h]
      -> ReactElementM h a
      -> ReactElementM h a
menu_ = semanticUI "Menu"

menuItem_ :: [PropertyOrHandler h]
          -> ReactElementM h a
          -> ReactElementM h a
menuItem_ = semanticUI "MenuItem"

menuMenu_ :: [PropertyOrHandler h]
          -> ReactElementM h a
          -> ReactElementM h a
menuMenu_ = semanticUI "MenuMenu"


form_ :: [PropertyOrHandler h] -> ReactElementM h a -> ReactElementM h a
form_ = semanticUI "Form"

formField_ :: [PropertyOrHandler h] -> ReactElementM h a -> ReactElementM h a
formField_ = semanticUI "FormField"

formGroup_ :: [PropertyOrHandler h] -> ReactElementM h a -> ReactElementM h a
formGroup_ = semanticUI "FormGroup"

segment_ :: [PropertyOrHandler h] -> ReactElementM h a -> ReactElementM h a
segment_ = semanticUI "Segment"

input_ :: [PropertyOrHandler h] -> ReactElementM h ()
input_ props = semanticUI "Input" props mempty

button_ :: [PropertyOrHandler h] -> ReactElementM h a -> ReactElementM h a
button_ = semanticUI "Button"

message_ :: [PropertyOrHandler h] -> ReactElementM h a -> ReactElementM h a
message_ = semanticUI "Message"

radio_ :: [PropertyOrHandler h] -> ReactElementM h ()
radio_ props = semanticUI "Radio" props mempty

checkbox_ :: [PropertyOrHandler h] -> ReactElementM h ()
checkbox_ props = semanticUI "Checkbox" props mempty

label_ :: [PropertyOrHandler h] -> ReactElementM h () -> ReactElementM h ()
label_ = semanticUI "Label"



onChangeSUI :: CallbackFunction handler handler
            => (JSVal -> JSVal -> handler) -> PropertyOrHandler handler
onChangeSUI = callback "onChange"


foreign import javascript unsafe
    "$1[$2]"
    js_getProp :: JSVal -> JSString -> JSVal

foreign import javascript unsafe
    "console.log($1)"
    js_dump :: JSVal -> IO ()

-- | Access a property from an object.  Since event objects are immutable, we
-- can use unsafePerformIO without worry.
(.:) :: FromJSVal b => JSVal -> JSString -> b
obj .: key = fromMaybe (error "Unable to decode event target")
           . unsafePerformIO .  fromJSVal $ js_getProp obj key

