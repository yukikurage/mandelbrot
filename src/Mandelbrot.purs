module Mandelbrot.Mandelbrot where

import Prelude

import Color (Color, fromHexString, rgb, rgb', toHSVA, toHexString)
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Traversable (sequence)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect)
import Effect.Exception (throw)
import Graphics.Canvas (CanvasElement, getCanvasElementById)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML (IProp, col)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks (HookM, StateId)
import Halogen.Hooks as Hooks
import Mandelbrot.Hooks.UseWindowSize (useWindowSize)
import Math (e)
import Web.Event.Event (currentTarget)
import Web.HTML.HTMLInputElement (value, fromEventTarget)

type ColorData = {h :: Number, s :: Number, v :: Number}

foreign import drawMandelbrot :: CanvasElement -> Effect Unit
foreign import shoot :: CanvasElement -> Effect Unit
foreign import setColorInner :: ColorData -> Effect Unit
foreign import setColorOuterZero :: ColorData -> Effect Unit
foreign import setColorOuterMax :: ColorData -> Effect Unit

type ViewerOffset = {position :: Position, scale :: Number} -- 真ん中の複素数, scale: 拡大率
type CanvasSize = {width :: Int, height :: Int}
type Position = {x:: Int, y :: Int}

toColorData :: Color -> ColorData
toColorData col = {h: c.h / 360.0, s: c.s, v: c.v}
  where
  c = toHSVA $ col

canvasSize :: {height :: Int, width :: Int}
canvasSize = {width: 600, height: 600}

component :: forall q i o. H.Component q i o Aff
component = Hooks.component \_ _ -> Hooks.do
  colorInner /\ colorInnerId <- Hooks.useState $ rgb' 1.0 1.0 1.0
  colorOuterZero /\ colorOuterZeroId <- Hooks.useState $ rgb' 1.0 1.0 1.0
  colorOuterMax /\ colorOuterMaxId <- Hooks.useState $ rgb' 0.0 0.0 0.0

  Hooks.useLifecycleEffect do
    liftEffect do
      canvas <- maybe (throw "canvas要素が取得できませんでした") pure =<< getCanvasElementById "canvas"
      drawMandelbrot canvas
    pure Nothing

  windowSize <- fromMaybe {width:640, height:640} <$> useWindowSize

  Hooks.pure $ HH.div [HP.style "width: 100vw; height: 100vh; position: relative;"] $
    [ HH.canvas
      [ HP.id "canvas"
      , HP.style $ "position: absolute;" <> "width: " <> show windowSize.width <> "px; height: " <> show windowSize.height <> "px;"
      , HP.width $ floor $ toNumber windowSize.width * 1.0
      , HP.height $ floor $ toNumber windowSize.height * 1.0
      ]
    , HH.i
      [ css "fas fa-camera"
      , HP.style "right: 40px; bottom: 10px; font-size: 20px; position: absolute; cursor: pointer; width: 50px; height: 50px; display: flex; align-items: center; justify-content: center;"
      , HE.onClick \_ -> liftEffect do
        canvas <- maybe (throw "canvas要素が取得できませんでした") pure =<< getCanvasElementById "canvas"
        shoot canvas
      ] []
    , HH.div
      [ HP.style "right: 40px; top: 10px; font-size: 15px; font-family: 'Montserrat', sans-serif; position: absolute; width: auto; height: 50px; display: flex; align-items: center; justify-content: center;"]
      [
        HH.text "YUKIWORKS"
      , HH.a
        [ HP.href "https://yukikurage.github.io/portfolio/"
        , HP.style "position:absolute; width: 100%; height: 100%; left: 0; top: 0;"
        , HP.target "_blank"
        , HP.rel "noopener"
        ][]
      ]
    , HH.div
      [ HP.style "left: 40px; top: 10px; font-size: 20px; font-family: 'Montserrat', sans-serif; position: absolute; width: auto; height: 50px; display: flex; align-items: center; justify-content: center;"]
      [
        HH.text "MANDELBROT"
      ]
    , HH.div
      [ HP.style "left: 40px; bottom: 10px; font-size: 20px; position: absolute; height: auto; margin: 10px 10px 10px 10px; display: flex; flex-direction: column; font-family: 'Montserrat', sans-serif; font-size: 15px;"]
      [ makeColorSetter setColorInner "#ffffff" "Inner" colorInnerId colorInner
      , makeColorSetter setColorOuterZero "#ffffff" "Outer Zero" colorOuterZeroId colorOuterZero
      , makeColorSetter setColorOuterMax "#000000" "Outer Max" colorOuterMaxId colorOuterMax
      ]
    ]

makeColorSetter :: forall w. (ColorData -> Effect Unit) -> String -> String -> StateId Color -> Color -> HH.HTML w (HookM Aff Unit)
makeColorSetter setter val tex id color = HH.div [HP.style $ "display: flex; flex-direction: row; align-items: center;"]
  [ HH.div
    [ HP.style $ "margin: 10px 10px 10px 10px; width: 18px; height: 18px; border: solid 1px #000000; background-color:" <> toHexString color <> ";"
    ] []
  , HH.text tex
  , HH.input
    [ HP.style "margin: 10px 10px 10px 10px; inherit; background-color:rgba(255,255,255,0); border: 0px solid; font-family: 'Montserrat', sans-serif; font-size: 18px;"
    , HP.value val
    , HE.onInput \e -> do
        col <- liftEffect do
          let
            input = fromEventTarget =<< currentTarget e
          colText <- sequence $ (value <$> input)
          let
            col = fromHexString =<< colText
          maybe (pure unit) (setter <<< toColorData) col
          pure col
        maybe (pure unit) (Hooks.put id) col
    ]
  ]

css :: forall r m. String -> IProp (class :: String | r) m
css str = HP.class_ $ H.ClassName str