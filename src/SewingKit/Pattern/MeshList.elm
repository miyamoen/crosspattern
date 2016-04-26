module SewingKit.Pattern.MeshList
  ( MeshList
  , init
  , xs, ys, width, height, positions
  , minX, minY, maxX, maxY
  , expand, shrink
  , Action(..), update
  ) where

import List.Extra exposing (replaceIf)

import SewingKit.Pattern.Mesh as Mesh exposing (Position, Mesh)

(=>) = (,)

(?) = flip Maybe.withDefault


type alias MeshList content =
  List (Mesh content)



init : MeshList content
init =
  expand 5 4 []


-- Action

type Action content
  = Expand Int Int
  | Shrink Int Int
  | Modify Position (Mesh.Action content)
  | NoOp


update : Action content -> MeshList content -> MeshList content
update action sqrs =
  case action of
    Expand dx dy ->
      expand dx dy sqrs

    Shrink dx dy ->
      shrink dx dy sqrs

    Modify pos sub ->
      let
        updateMesh sqr =
          if Mesh.position sqr == pos then
            Mesh.update sub sqr
          else
            sqr

      in
        List.map updateMesh sqrs

    NoOp ->
      sqrs



-- position

positions : MeshList content -> List Position
positions =
  List.map Mesh.position


-- x

xs : MeshList content -> List Int
xs =
  List.map Mesh.x


maxX : MeshList content -> Maybe Int
maxX =
  xs >> List.maximum


minX : MeshList content -> Maybe Int
minX =
  xs >> List.minimum


width : MeshList content -> Int
width sqrs =
  Maybe.map2 (\max min -> max - min + 1) (maxX sqrs) (minX sqrs) ? 0


-- y

ys : MeshList content -> List Int
ys =
  List.map Mesh.y


maxY : MeshList content -> Maybe Int
maxY =
  ys >> List.maximum


minY : MeshList content -> Maybe Int
minY =
  ys >> List.minimum


height : MeshList content -> Int
height sqrs =
  Maybe.map2 (\max min -> max - min + 1) (maxY sqrs) (minY sqrs) ? 0


-- expand & shrink

expand : Int -> Int -> MeshList content -> MeshList content
expand dx dy sqrs =
  expandOnX dx sqrs
    |> expandOnY dy


shrink : Int -> Int -> MeshList content -> MeshList content
shrink dx dy sqrs =
  shrinkOnX dx sqrs
    |> shrinkOnY dy


expandOnX : Int -> MeshList content -> MeshList content
expandOnX dx sqrs =
  case ( compare dx 0, maxX sqrs, minX sqrs ) of
    ( GT, Just max, _ ) ->
      List.map (Mesh.init <| max + 1) (ys sqrs) ++ sqrs
        |> expandOnX (dx - 1)

    ( LT, _, Just min ) ->
      List.map (Mesh.init <| min - 1) (ys sqrs) ++ sqrs
        |> expandOnX (dx + 1)

    ( EQ, _, _ ) ->
      sqrs

    ( _, _, _ ) ->
      expandOnX ( dx // (abs dx) * (abs dx - 1) ) [ Mesh.init 0 0 ]


shrinkOnX : Int -> MeshList content -> MeshList content
shrinkOnX dx sqrs =
  case ( compare dx 0, maxX sqrs, minX sqrs ) of
    ( GT, Just max, _ ) ->
      List.filter (\sqr -> max /= Mesh.x sqr) sqrs
        |> shrinkOnX (dx - 1)

    ( LT, _, Just min ) ->
      List.filter (\sqr -> min /= Mesh.x sqr) sqrs
        |> shrinkOnX (dx + 1)

    ( _, _, _ ) ->
      sqrs


expandOnY : Int -> MeshList content -> MeshList content
expandOnY dy sqrs =
  case ( compare dy 0, maxY sqrs, minY sqrs ) of
    ( GT, Just max, _ ) ->
      List.map (flip Mesh.init <| max + 1) (xs sqrs) ++ sqrs
        |> expandOnY (dy - 1)

    ( LT, _, Just min ) ->
      List.map (flip Mesh.init <| min - 1) (xs sqrs) ++ sqrs
        |> expandOnY (dy + 1)

    ( EQ, _, _ ) ->
      sqrs

    ( _, _, _ ) ->
      expandOnY ( dy // (abs dy) * (abs dy - 1) ) [ Mesh.init 0 0 ]


shrinkOnY : Int -> MeshList content -> MeshList content
shrinkOnY dy sqrs =
  case ( compare dy 0 , maxY sqrs, minY sqrs ) of
    ( GT, Just max, _ ) ->
      List.filter (\sqr -> max /= Mesh.y sqr) sqrs
        |> shrinkOnY (dy - 1)

    ( LT, _, Just min ) ->
      List.filter (\sqr -> min /= Mesh.y sqr) sqrs
        |> shrinkOnY (dy + 1)

    ( _, _, _ ) ->
      sqrs


