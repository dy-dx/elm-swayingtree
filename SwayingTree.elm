module SwayingTree where

import Signal
import List
import Graphics.Element (Element, empty)
import Graphics.Collage (..)
import Color
import Window
import Mouse
import Time
import Noise.ProcNoise
--import Debug

-- len, weight, left, right
type Tree = Empty | Node Float Float Tree Tree


makeTree : Float -> Float -> Tree
makeTree len weight =
  if | len < 18.0 -> Empty
     | otherwise ->
        let nextLen = len * 0.8
            nextWeight = weight * 0.75
        in
          Node len weight
            (makeTree nextLen nextWeight)
            (makeTree nextLen nextWeight)


branches : Tree -> Float -> Float -> Float -> Float -> Float -> List Form
branches t x y rotation theta wind = case t of
  Empty -> [toForm empty]
  Node len weight left right ->
    let endPtX = x + (len * (cos rotation)) / 2
        endPtY = y + (len * (sin rotation)) / 2
        nextTheta = (theta * 0.86)
        rightRot = rotation + wind + nextTheta
        leftRot  = rotation + wind - nextTheta
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


tWind : Signal Float
tWind =
  Signal.foldp (\ms total -> total + 0.006) 0 (Time.fps 60)

signalWind : Signal Float
signalWind =
  Signal.map
    (\time -> (Noise.ProcNoise.perlin 0 time) * 2 - 1 |> (*) 0.4)
    tWind


drawTree : Float -> Float -> Tree -> Form
drawTree theta wind t =
  branches t 0 0 (pi/2) theta wind
    |> group
    |> move (0, -150)


view : (Int, Int) -> (Int, Int) -> Float -> Element
view (w, h) (mx, my) wind =
  let theta = max ((toFloat my) / (toFloat h)) 0.25
  in collage w h
    [
      makeTree 80 8 |> drawTree theta wind
    ]


main : Signal Element
main =
  Signal.map3 view Window.dimensions Mouse.position signalWind
