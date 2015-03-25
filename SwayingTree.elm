module SwayingTree where

import Random
import Signal
import List
import Graphics.Element (Element, empty)
import Graphics.Collage (..)
import Color
import String
import Window
import Mouse
import Time
--import Text (..)
import Debug

-- len, weight, left, right
type Tree = Empty | Node Float Float Tree Tree
initWind = 0


tree : Float -> Float -> Tree
tree len weight =
  if | len < 18.0 -> Empty
     | otherwise ->
        let nextLen = len * 0.8
            nextWeight = weight * 0.75
        in
          Node len weight
            (tree nextLen nextWeight)
            (tree nextLen nextWeight)


branches : Tree -> Float -> Float -> Float -> Float -> Float -> List Form
branches t x y rotation theta wind = case t of
  Empty -> [toForm empty]
  Node len weight left right ->
    let endPtX = x + (len * (cos rotation)) / 2
        endPtY = y + (len * (sin rotation)) / 2
        nextTheta = (theta * 0.86)
        rightRot = rotation + nextTheta
        leftRot  = rotation - nextTheta
    in
    ( rect len weight
        |> filled Color.black
        |> move (x + endPtX, y + endPtY)
        |> rotate rotation
    ) :: (
      (branches right endPtX endPtY rightRot nextTheta wind)
      ++
      (branches left  endPtX endPtY leftRot  nextTheta wind)
    )


draw : (Float) -> Form
draw (theta) =
  let length = 80
      weight = 8
      wind = 0.0
      theTree = (tree length weight)
  in
    branches theTree 0 0 (pi/2) theta wind
      |> group
      |> move (0, -150.0)


view : (Int, Int) -> (Int, Int) -> Element
view (w, h) (mx, my) =
  let theta = (toFloat (h - my)) / (toFloat h)
  in collage w h
    [
      (draw theta)
    ]


main : Signal Element
main =
  Signal.map2 view Window.dimensions Mouse.position
