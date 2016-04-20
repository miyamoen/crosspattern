module SewingKit.Pattern.SquareList
  ( SquareList
  , init
  , xs, ys, width, height, positions
  , expand, shrink
  , Action(..), update
  ) where

import List.Extra exposing (replaceIf)

import SewingKit.Pattern.Square as Square exposing (Position, Square)

(=>) = (,)

(?) = flip Maybe.withDefault


type alias SquareList content =
  List (Square content)



init : SquareList content
init =
  expand 10 10 []


-- Action

type Action content
  = Expand Int Int
  | Shrink Int Int
  | Modify Position (Square.Action content)
  | NoOp


update : Action content -> SquareList content -> SquareList content
update action sqrs =
  case action of
    Expand dx dy ->
      expand dx dy sqrs

    Shrink dx dy ->
      shrink dx dy sqrs

    Modify pos sub ->
      let
        updateSquare sqr =
          if Square.position sqr == pos then
            Square.update sub sqr
          else
            sqr

      in
        List.map updateSquare sqrs

    NoOp ->
      sqrs



-- position

positions : SquareList content -> List Position
positions =
  List.map Square.position


-- x

xs : SquareList content -> List Int
xs =
  List.map Square.x


maxX : SquareList content -> Maybe Int
maxX =
  xs >> List.maximum


minX : SquareList content -> Maybe Int
minX =
  xs >> List.minimum


width : SquareList content -> Int
width sqrs =
  Maybe.map2 (\max min -> max - min + 1) (maxX sqrs) (minX sqrs) ? 0


-- y

ys : SquareList content -> List Int
ys =
  List.map Square.y


maxY : SquareList content -> Maybe Int
maxY =
  ys >> List.maximum


minY : SquareList content -> Maybe Int
minY =
  ys >> List.minimum


height : SquareList content -> Int
height sqrs =
  Maybe.map2 (\max min -> max - min + 1) (maxY sqrs) (minY sqrs) ? 0


-- expand & shrink

expand : Int -> Int -> SquareList content -> SquareList content
expand dx dy sqrs =
  expandOnX dx sqrs
    |> expandOnY dy


shrink : Int -> Int -> SquareList content -> SquareList content
shrink dx dy sqrs =
  shrinkOnX dx sqrs
    |> shrinkOnY dy


expandOnX : Int -> SquareList content -> SquareList content
expandOnX dx sqrs =
  case ( compare dx 0, maxX sqrs, minX sqrs ) of
    ( GT, Just max, _ ) ->
      List.map (Square.init <| max + 1) (ys sqrs) ++ sqrs
        |> expandOnX (dx - 1)

    ( LT, _, Just min ) ->
      List.map (Square.init <| min - 1) (ys sqrs) ++ sqrs
        |> expandOnX (dx + 1)

    ( EQ, _, _ ) ->
      sqrs

    ( _, _, _ ) ->
      expandOnX ( dx // (abs dx) * (abs dx - 1) ) [ Square.init 0 0 ]


shrinkOnX : Int -> SquareList content -> SquareList content
shrinkOnX dx sqrs =
  case ( compare dx 0, maxX sqrs, minX sqrs ) of
    ( GT, Just max, _ ) ->
      List.filter (\sqr -> max /= Square.x sqr) sqrs
        |> shrinkOnX (dx - 1)

    ( LT, _, Just min ) ->
      List.filter (\sqr -> min /= Square.x sqr) sqrs
        |> shrinkOnX (dx + 1)

    ( _, _, _ ) ->
      sqrs


expandOnY : Int -> SquareList content -> SquareList content
expandOnY dy sqrs =
  case ( compare dy 0, maxY sqrs, minY sqrs ) of
    ( GT, Just max, _ ) ->
      List.map (flip Square.init <| max + 1) (xs sqrs) ++ sqrs
        |> expandOnY (dy - 1)

    ( LT, _, Just min ) ->
      List.map (flip Square.init <| min - 1) (xs sqrs) ++ sqrs
        |> expandOnY (dy + 1)

    ( EQ, _, _ ) ->
      sqrs

    ( _, _, _ ) ->
      expandOnY ( dy // (abs dy) * (abs dy - 1) ) [ Square.init 0 0 ]


shrinkOnY : Int -> SquareList content -> SquareList content
shrinkOnY dy sqrs =
  case ( compare dy 0 , maxY sqrs, minY sqrs ) of
    ( GT, Just max, _ ) ->
      List.filter (\sqr -> max /= Square.y sqr) sqrs
        |> shrinkOnY (dy - 1)

    ( LT, _, Just min ) ->
      List.filter (\sqr -> min /= Square.y sqr) sqrs
        |> shrinkOnY (dy + 1)

    ( _, _, _ ) ->
      sqrs


