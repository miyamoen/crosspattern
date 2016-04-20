module Cloth where

import Color exposing (Color, red, orange, yellow, green, blue, purple, brown, charcoal, grey, black)
import Random exposing (Generator, generate, initialSeed)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Array exposing (Array)

-- MODEL
type alias Point = { x:Int, y:Int }
type alias ColorNumber = Int
type alias Square = { color:Maybe Color, loc:Point }
type alias Pattern = { center:Point, squares:List Square, colors:List Color }
type alias Cloth = { center:Point, squares:List Square, patterns:List Pattern }

main : Element
main = collage 50 50
  [ clothForm bold (colorfulCloth 5 5)
  ]

bold : Float
bold = 4
squareLength : Float
squareLength = 15

colors : Array.Array Color
colors = Array.fromList [red, orange, yellow, green, blue, purple, brown, charcoal, grey, black]

randomColor : Generator Color
randomColor = Random.map (\n -> Maybe.withDefault orange (Array.get n colors)) (Random.int 0 (List.length colors))

randomColors : Int -> Generator (List Color)
randomColors n = Random.list n randomColor

clothForm : Float -> Cloth -> Form
clothForm bold cloth = squaresForm bold cloth.squares

colorfulCloth : Int -> Int -> Cloth
colorfulCloth width height =
  let (colors, _) = generate (randomColors (width * height)) (initialSeed 234135)
      points      = rectPoints width height
      squares = List.map2 (\c p -> { color = Just c, loc = p }) colors points
  in { center = { x = 0, y = 0 }, patterns = [], squares = squares }

rectPoints : Int -> Int -> List Point
rectPoints w h =
  let ws = [-(w-1)//2..w//2]
      hs = [-(h-1)//2..h//2]
  in product ws hs |> List.map (\(w,h) -> { x = w, y = h })

product : List a -> List b -> List (a,b)
product xs ys =
  let andThen = flip List.concatMap
  in
    xs `andThen` \x ->
    ys `andThen` \y ->
    [(x,y)]

cross : Float -> Color -> Form
cross bold color =
  let tracedS = traced { defaultLine | color = color, cap = Round , width = bold }
  in group [ tracedS (segment (5,5) (-5,-5)), tracedS (segment (-5,5) (5,-5)) ]

pointToFloat : Point -> (Float,Float)
pointToFloat point = (toFloat point.x*squareLength,toFloat point.y*squareLength)

moveToPoint: Point -> Form -> Form
moveToPoint point form = move (pointToFloat point) form

squareForm : Float -> Square -> Maybe Form
squareForm bold square =
  case square.color of
    Just color -> cross bold color |> moveToPoint square.loc |> Just
    Nothing -> Nothing

squaresForm : Float -> List Square -> Form
squaresForm bold squares = List.filterMap (squareForm bold) squares |> group