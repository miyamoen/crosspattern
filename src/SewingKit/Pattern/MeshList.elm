module SewingKit.Pattern.MeshList
  ( MeshList
  , init
  , xs, ys, width, height, positions
  , minX, minY, maxX, maxY
  , expand, shrink
  , Action(..), update
  , gridElement
  ) where

import List.Extra exposing (replaceIf)
import Color

import SewingKit.Svg exposing (Element(..), Cap(..), Join(..))
import SewingKit.Pattern.Mesh as Mesh exposing (Position, Mesh)

(=>) = (,)

(?) = flip Maybe.withDefault


type alias MeshList content =
  List (Mesh content)



init : MeshList content
init =
  expand 10 9 []


-- Action

type Action content
  = Expand Int Int
  | Shrink Int Int
  | Modify Position (Mesh.Action content)
  | NoOp


update : Action content -> MeshList content -> MeshList content
update action mshs =
  case action of
    Expand dx dy ->
      expand dx dy mshs

    Shrink dx dy ->
      shrink dx dy mshs

    Modify pos sub ->
      let
        updateMesh msh =
          if Mesh.position msh == pos then
            Mesh.update sub msh
          else
            msh

      in
        List.map updateMesh mshs

    NoOp ->
      mshs



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
width mshs =
  Maybe.map2 (\max min -> max - min + 1) (maxX mshs) (minX mshs) ? 0


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
height mshs =
  Maybe.map2 (\max min -> max - min + 1) (maxY mshs) (minY mshs) ? 0


-- expand & shrink

expand : Int -> Int -> MeshList content -> MeshList content
expand dx dy mshs =
  expandOnX dx mshs
    |> expandOnY dy


shrink : Int -> Int -> MeshList content -> MeshList content
shrink dx dy mshs =
  shrinkOnX dx mshs
    |> shrinkOnY dy


expandOnX : Int -> MeshList content -> MeshList content
expandOnX dx mshs =
  case ( compare dx 0, maxX mshs, minX mshs ) of
    ( GT, Just max, _ ) ->
      List.map (Mesh.init <| max + 1) (ys mshs) ++ mshs
        |> expandOnX (dx - 1)

    ( LT, _, Just min ) ->
      List.map (Mesh.init <| min - 1) (ys mshs) ++ mshs
        |> expandOnX (dx + 1)

    ( EQ, _, _ ) ->
      mshs

    ( _, _, _ ) ->
      expandOnX ( dx // (abs dx) * (abs dx - 1) ) [ Mesh.init 0 0 ]


shrinkOnX : Int -> MeshList content -> MeshList content
shrinkOnX dx mshs =
  case ( compare dx 0, maxX mshs, minX mshs ) of
    ( GT, Just max, _ ) ->
      List.filter (\msh -> max /= Mesh.x msh) mshs
        |> shrinkOnX (dx - 1)

    ( LT, _, Just min ) ->
      List.filter (\msh -> min /= Mesh.x msh) mshs
        |> shrinkOnX (dx + 1)

    ( _, _, _ ) ->
      mshs


expandOnY : Int -> MeshList content -> MeshList content
expandOnY dy mshs =
  case ( compare dy 0, maxY mshs, minY mshs ) of
    ( GT, Just max, _ ) ->
      List.map (flip Mesh.init <| max + 1) (xs mshs) ++ mshs
        |> expandOnY (dy - 1)

    ( LT, _, Just min ) ->
      List.map (flip Mesh.init <| min - 1) (xs mshs) ++ mshs
        |> expandOnY (dy + 1)

    ( EQ, _, _ ) ->
      mshs

    ( _, _, _ ) ->
      expandOnY ( dy // (abs dy) * (abs dy - 1) ) [ Mesh.init 0 0 ]


shrinkOnY : Int -> MeshList content -> MeshList content
shrinkOnY dy mshs =
  case ( compare dy 0 , maxY mshs, minY mshs ) of
    ( GT, Just max, _ ) ->
      List.filter (\msh -> max /= Mesh.y msh) mshs
        |> shrinkOnY (dy - 1)

    ( LT, _, Just min ) ->
      List.filter (\msh -> min /= Mesh.y msh) mshs
        |> shrinkOnY (dy + 1)

    ( _, _, _ ) ->
      mshs


gridElement : MeshList content -> Element
gridElement mshs =
  let
    w = width mshs
    h = height mshs
    x = minX mshs ? 0 |> toFloat
    y = minY mshs ? 0 |> toFloat
  in
    Grid 1.0 w h x y
    |> LineStyle Color.blue 0.08
    |> Opacity 0.3 0.5
    |> LineCap RoundCap
    |> DashStyle [ 0.1, 0.1 ] -0.05