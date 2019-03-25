module Toggle where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Store (Store, store)
import Control.Monad.Free (Free, foldFree, liftF)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Renderless.State (getState, modifyState_, modifyStore)

type Component o item m = H.Component HH.HTML (Query o item) (Input o item) (Message o item) m
type ComponentHTML o item = H.ComponentHTML (Query o item)
type ComponentDSL o item m = H.ComponentDSL (StateStore o item) (Query o item) (Message o item) m
type StateStore o item = Store (State item) (ComponentHTML o item)

data QueryF o item a
  = SetToggle Toggle a
  | GetToggle (Toggle -> a)
  | Raise (o Unit) a
  | Receive (Input o item) a

type Query o item = Free (QueryF o item)

always :: ∀ a b. a -> b -> Maybe a
always = const <<< Just

setToggle :: ∀ o item . Toggle -> Query o item Unit
setToggle t = liftF (SetToggle t unit)

getToggle :: ∀ o item . Query o item Toggle
getToggle = liftF (GetToggle identity)

toggleToggle :: ∀ o item . Query o item Unit
toggleToggle = getToggle >>= not >>> setToggle

raise :: ∀ o item . o Unit -> Query o item Unit
raise o = liftF (Raise o unit)

receive :: ∀ o item . Input o item -> Query o item Unit
receive i = liftF (Receive i unit)

data Toggle = Off | On
derive instance eqToggle :: Eq Toggle
derive instance ordToggle :: Ord Toggle

instance heytingAlgebraToggle :: HeytingAlgebra Toggle where
  tt = On
  ff = Off
  not On = Off
  not Off = On
  conj On On = On
  conj _ _ = Off
  disj Off Off = Off
  disj _ _ = On
  implies On Off = Off
  implies _ _ = On
instance booleanAlgebraToggle :: BooleanAlgebra Toggle

type State item =
  { toggle :: Toggle }

type Input o item =
  { render :: State item -> ComponentHTML o item }

data Message o item
  = ToggleChanged Toggle
  | Emit (o Unit)

component :: ∀ o item m. MonadAff m => Component o item m
component =
  H.lifecycleComponent
    { initialState
    , render: extract
    , eval: eval'
    , receiver: Just <<< receive
    , initializer: Nothing
    , finalizer: Nothing
    }
  where
    initialState i = store i.render
      { toggle: Off
      }

    eval' :: Query o item ~> ComponentDSL o item m
    eval' a = foldFree eval a

    -- Helper for setting Toggle inside `eval`. Eta-expanded bc strict
    -- mutual recursion woes.
    setTog t = eval' (setToggle t)

    eval :: QueryF o item ~> ComponentDSL o item m
    eval = case _ of
      SetToggle t a -> a <$ do
        st <- getState
        when (st.toggle /= t) do
          modifyState_ _ { toggle = t }
          H.raise $ ToggleChanged t

      GetToggle f -> do
        st <- getState
        pure (f st.toggle)

      Raise parentQuery a -> a <$ do
        H.raise (Emit parentQuery)

      Receive input a -> a <$ do
        modifyStore input.render identity