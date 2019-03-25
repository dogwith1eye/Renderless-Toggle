module Toggle.Setters where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Toggle (Query)
import Toggle as Toggle
import Web.UIEvent.MouseEvent as ME

type ToggleProps p =
  ( onClick :: ME.MouseEvent
  | p
  )

setToggleProps
  :: ∀ o item p
   . Array (HP.IProp (ToggleProps p) (Query o item Unit))
  -> Array (HP.IProp (ToggleProps p) (Query o item Unit))
setToggleProps = (<>)
  [ HE.onClick \ev -> Just do
      Toggle.getToggle >>= case _ of
        Toggle.On -> do
          Toggle.setToggle Toggle.Off
        Toggle.Off -> do
          Toggle.setToggle Toggle.On
  ]