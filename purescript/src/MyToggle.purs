module MyToggle where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Toggle as Toggle
import Toggle.Setters as Setters

data Query a
  = HandleToggle (Toggle.Message Query String) a
  | Reset a

type State =
  { toggleCount :: Int }

type Input = Unit

type Message = Void

type ChildSlot = Unit
type ChildQuery = Toggle.Query Query String

component :: ∀ m. MonadAff m => H.Component HH.HTML Query Input Message m
component =
  H.parentComponent
    { initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: Input -> State
  initialState = const
    { toggleCount: 0 }

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
  render st =
    HH.div_
    [ HH.h1_
      [ HH.text ("Toggle " <> show st.toggleCount) ]
    , HH.slot unit Toggle.component toggleInput (HE.input HandleToggle)
    ]
    where
      toggleInput :: Toggle.Input Query String
      toggleInput =
        { render: toggle }

      toggle :: Toggle.State String -> Toggle.ComponentHTML Query String
      toggle cst =
        HH.div_
        [ HH.button
          [ HE.onClick $ Toggle.always $ Toggle.setToggle Toggle.Off ]
          [ HH.text "Blue Pill" ]
        , HH.button
          [ HE.onClick $ Toggle.always $ Toggle.setToggle Toggle.On ]
          [ HH.text "Red Pill" ]
        , HH.button
          [ HE.onClick $ Toggle.always $ Toggle.toggleToggle ]
          [ HH.text "Other Pill" ]
        , HH.button
          (Setters.setToggleProps [])
          [ HH.text "Default Pill" ]
        , HH.button
          [ HE.onClick $ Toggle.always $ Toggle.raise $ Reset unit ]
          [ HH.text "Reset Pills" ]
        , HH.div_
          [ if cst.toggle == Toggle.On
              then HH.text "It's all a dream, go back to sleep."
              else HH.text "I don't know how far the rabbit hole goes, I'm not a rabbit, neither do I measure holes."
          ]
        ]

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Message m
  eval = case _ of
    Reset next -> do
        _ <- H.modify \st -> st { toggleCount = 0 }
        pure next
    HandleToggle message next -> case message of
      Toggle.ToggleChanged tog -> do
        _ <- H.modify \st -> st { toggleCount = st.toggleCount + 1 }
        pure next

      Toggle.Emit query -> eval query *> pure next
