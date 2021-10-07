module Mandelbrot.Hooks.UseWindowSize where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.Hooks (type (<>), Hook, HookM, UseEffect, UseState)
import Halogen.Hooks as Hooks
import Halogen.Query.Event as HE
import Web.Event.Event (EventType(..))
import Web.Event.Event as Event
import Web.HTML as HTML
import Web.HTML.Window as Window

type UseWindowSize = UseState (Maybe {width :: Int, height :: Int}) <> UseEffect <> Hooks.Pure

useWindowSize :: forall m. MonadAff m => Hook m UseWindowSize (Maybe {width :: Int, height :: Int})
useWindowSize = Hooks.do
    size /\ sizeId <- Hooks.useState Nothing

    Hooks.useLifecycleEffect do
      subscription <- subscribe (Hooks.put sizeId)
      pure $ Just $ Hooks.unsubscribe subscription

    Hooks.pure size
    where
    subscribe :: (Maybe {width :: Int, height :: Int} -> HookM m Unit) -> HookM m H.SubscriptionId
    subscribe setSize = do
      let
        readSize win = do
          w <- liftEffect $ Window.innerWidth win
          h <- liftEffect $ Window.innerHeight win
          setSize $ Just {width: w, height: h}

      window <- liftEffect HTML.window
      subscriptionId <- Hooks.subscribe do
        HE.eventListener
          (EventType "resize")
          (Window.toEventTarget window)
          (Event.target >=> Window.fromEventTarget >>> map readSize)

      readSize window
      pure subscriptionId