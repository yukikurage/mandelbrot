module Mandelbrot where

import Data.Complex
import Prelude

import Data.List.Lazy (find, findIndex, iterate, take)
import Data.Maybe (Maybe(..))

data Result = Convergence | Divergence Int --発散/あるいは収束までの速度

type Times = Int

type Mandelbrot = Times -> Complex Number -> Result

mandelbrot :: Mandelbrot
mandelbrot n c = case result of
  Nothing -> Convergence
  Just i -> Divergence i
  where
    threshold = 2.0
    rec z = z * z + c
    itr = take n $ iterate rec $ Complex 0.0 0.0
    result = findIndex (\w -> magnitude w > threshold) itr

