module Mandelbrot.Mandelbrot where

import Prelude

import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Exception (throw)
import Graphics.Canvas (CanvasElement, getCanvasElementById)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Mandelbrot.Hooks.UseWindowSize (useWindowSize)

foreign import drawMandelbrot :: CanvasElement -> Effect Unit
foreign import shoot :: CanvasElement -> Effect Unit

type ViewerOffset = {position :: Position, scale :: Number} -- 真ん中の複素数, scale: 拡大率
type CanvasSize = {width :: Int, height :: Int}
type Position = {x:: Int, y :: Int}

canvasSize :: { height :: Int, width :: Int}
canvasSize = {width: 600, height: 600}

component :: forall q i o. H.Component q i o Aff
component = Hooks.component \_ _ -> Hooks.do
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
      [ HP.style "left: 40px; bottom: 10px; font-size: 15px; font-family: 'Montserrat', sans-serif; position: absolute; width: auto; height: 50px; display: flex; align-items: center; justify-content: center;"]
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
    ]

css :: forall r m. String -> IProp (class :: String | r) m
css str = HP.class_ $ H.ClassName str