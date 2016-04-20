module SewingKit.Pattern.Cloth
  ( Cloth, stitch ) where

import Html exposing (..)
import Html.Attributes exposing (style)
import Svg
import Color exposing (Color)
import Signal exposing (Address, message, Message, forwardTo)
import List.Extra exposing (lift2)
import Array exposing (indexedMap, toList)

import SewingKit.Svg exposing (..)
import SewingKit.Stitch as Stitch
import SewingKit.StitchList as Stitches
import SewingKit.Pattern.Squares as Squares exposing (Square)

(=>) = (,)

(?) = flip Maybe.withDefault
(?>) = Maybe.andThen


type alias Cloth =
  { stitches : Stitches.Model
  , squares : List Square
  }




stitch : Int -> Cloth -> Maybe Stitch.Cloth
stitch idx cloth =
  Stitches.stitch idx cloth.stitches


color : Int -> Cloth -> Maybe Color
color idx cloth =
  Stitches.color idx cloth.stitches


manner : Int -> Cloth -> Maybe Stitch.Manner
manner idx cloth =
  Stitches.manner idx cloth.stitches



init : Cloth
init =
  { stitches = Stitches.init
  , squares = Squares.init
  }


-- Action

type Action
  = ModifyStitch Stitches.Action
  | ModifySquares Squares.Action
  | NoOp


update : Action -> Cloth -> Cloth
update action cloth =
  case action of
    ModifyStitch sub ->
      { cloth | stitches = Stitches.update sub cloth.stitches }

    ModifySquares sub ->
      { cloth | squares = Squares.update sub cloth.squares }

    NoOp ->
      cloth


viewStitches : Address Action -> Cloth -> Html
viewStitches address cloth =
  Stitches.view (forwardTo address ModifyStitch) cloth.stitches


grid : Cloth -> Element -> Element
grid cloth elm =
  if cloth.hasGrid then
    LineStyle Color.black 0.02 elm
      |> DashStyle [ 0.1, 0.1 ] -0.05

  else
    elm


gridButton : Address Action -> Cloth -> Html
gridButton address cloth =
  let
    color =
      case cloth.hasGrid of
        True ->
          Color.hsl (degrees 100) 0.85 0.85
        False ->
          Color.hsl (degrees 0) 0.85 0.85

  in
    Circle 0 0 0.45
      |> Fill color
      |> Clickable (message address SwitchGrid)
      |> toHtml 1 1 50 50 []