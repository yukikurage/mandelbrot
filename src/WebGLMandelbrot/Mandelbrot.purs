module WebGLMandelbrot.Mandelbrot where

import Prelude

import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Exception (throw)
import Graphics.Canvas (CanvasElement, getCanvasElementById)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks

foreign import drawMandelbrot :: CanvasElement -> Effect Unit

type ViewerOffset = {position :: Position, scale :: Number} -- 真ん中の複素数, scale: 拡大率
type CanvasSize = {width :: Int, height :: Int}
type Position = {x:: Int, y :: Int}

canvasSize :: { height :: Int
, width :: Int
}
canvasSize = {width: 600, height: 600}

component :: forall q i o. H.Component q i o Aff
component = Hooks.component \_ _ -> Hooks.do
  Hooks.useLifecycleEffect do
    liftEffect do
      canvas <- maybe (throw "canvas要素が取得できませんでした") pure =<< getCanvasElementById "canvas"
      drawMandelbrot canvas
    pure Nothing

  Hooks.pure $ HH.canvas
    [ HP.id "canvas"
    , HP.style $ "width:" <> show canvasSize.width <> "px; height:" <> show canvasSize.height <> "px;"
    , HP.width $ floor $ toNumber canvasSize.width * 1.0
    , HP.height $ floor $ toNumber canvasSize.height * 1.0
    , HE.onClick \e -> do
      pure unit
    ]