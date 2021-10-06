module Viewer where

import Prelude

import Data.Array (concat, replicate, (..))
import Data.ArrayBuffer.Typed (set)
import Data.Complex (Complex(..), imag, real)
import Data.Int (ceil, floor, toNumber)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple.Nested ((/\))
import Data.UInt (fromInt)
import Effect.Aff (Aff)
import Effect.Exception (throw)
import Graphics.Canvas (getCanvasElementById, getCanvasHeight, getCanvasWidth, getContext2D, getImageData, imageDataBuffer, imageDataHeight, imageDataWidth, putImageData)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Mandelbrot (Result(..), mandelbrot)
import Web.Event.Event (target)
import Web.HTML.HTMLElement (fromEventTarget, getBoundingClientRect)
import Web.UIEvent.MouseEvent as MouseEvent

type ViewerOffset = {position :: Position, scale :: Number} -- 真ん中の複素数, scale: 拡大率
type CanvasSize = {width :: Int, height :: Int}
type Position = {x:: Int, y :: Int}

canvasSize :: CanvasSize
canvasSize = {width: 640, height: 640}

initialOffset :: ViewerOffset
initialOffset = {position: {x: canvasSize.width / 2, y: canvasSize.height / 2}, scale: 160.0}

loopNum :: Int
loopNum = 50

zoomScale = 1.2

positionToComplex :: ViewerOffset -> Position -> Complex Number
positionToComplex offset position = Complex
  (toNumber (position.x - offset.position.x) / offset.scale)
  (toNumber (offset.position.y - position.y) / offset.scale)

complexToPosition :: ViewerOffset -> Complex Number -> Position
complexToPosition offset z =
  { x: offset.position.x + floor (real z * offset.scale)
  , y: offset.position.y - floor (imag z * offset.scale)
  }

component :: forall q i o. H.Component q i o Aff
component = Hooks.component \_ _ -> Hooks.do
  offset /\ offsetId <- Hooks.useState initialOffset

  Hooks.captures {offset} Hooks.useTickEffect do
    liftEffect do
      canvas <- maybe (throw "canvas要素が取得できませんでした") pure =<< getCanvasElementById "canvas"
      context <- getContext2D canvas
      width <- getCanvasWidth canvas
      height <- getCanvasHeight canvas
      imageData <- getImageData context 0.0 0.0 width height
      let
        imgWidth = imageDataWidth imageData
        imgHeight = imageDataHeight imageData
        pixels = imageDataBuffer imageData
      _ <- set pixels Nothing $ concat $ do
        y <- 0 .. (imgHeight - 1)
        x <- 0 .. (imgWidth - 1)
        let
          z = positionToComplex offset {x: x, y: y}
        pure $ map fromInt $ case mandelbrot loopNum z of
          Convergence -> [0, 0, 0, 255]
          Divergence n -> replicate 3 ((255 * n) / loopNum) <> [255]
      putImageData context imageData 0.0 0.0
      pure unit
    pure Nothing

  Hooks.pure $ HH.canvas 
    [ HP.id "canvas"
    , HP.width canvasSize.width
    , HP.height canvasSize.height
    , HE.onClick \e -> do
      rect <- liftEffect
        $ fromMaybe (throw "Eventからのキャンバスの取得が失敗しました")
        $ getBoundingClientRect <$> (fromEventTarget =<< target (MouseEvent.toEvent e))
      let
        x = MouseEvent.clientX e - floor rect.left
        y = MouseEvent.clientY e - floor rect.top
        newPosition =
          { x: offset.position.x - floor (toNumber (x - canvasSize.width / 2) * zoomScale)
          , y: offset.position.y - floor (toNumber (y - canvasSize.height / 2) * zoomScale)
          }
        newOffset = {position: newPosition, scale: offset.scale * zoomScale}
      Hooks.put offsetId newOffset
      pure unit
    ]