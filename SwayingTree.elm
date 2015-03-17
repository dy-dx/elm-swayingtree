module SwayingTree where

import Random
import Signal
import List
import Graphics.Element (..)
import Graphics.Collage (..)
import Color
import String
import Window
import Mouse
import Time
import Text (..)
import Debug


view : (Int, Int) -> (Int, Int) -> Element
view (w, h) (mx, my) =
  collage w h
    [
      asText "Hello" |> toForm
    ]


main : Signal Element
main =
  Signal.map2 view Window.dimensions Mouse.position
