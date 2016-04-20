module SewingKit.Cloth
  ( Model, init, stitch, color, manner
  , Action, update, view) where

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

(=>) = (,)

(?) = flip Maybe.withDefault


type alias Model =
  { stitches : Stitches.Model
  , holding : Maybe Int
  , squares : List (Position, Maybe Int)
  , hasGrid : Bool
  }


type alias Position =
  (Int, Int)


stitch : Int -> Model -> Maybe Stitch.Model
stitch idx model =
  Stitches.stitch idx model.stitches


color : Int -> Model -> Maybe Color
color idx model =
  Stitches.color idx model.stitches


manner : Int -> Model -> Maybe Stitch.Manner
manner idx model =
  Stitches.manner idx model.stitches



init : Int -> Int -> Model
init w h =
  { stitches = Stitches.init
  , holding = Just 0
  , squares
    = lift2 (,) [0..(w - 1)] [0..(h - 1)]
    |> List.map ((flip (,)) Nothing)
  , hasGrid = True
  }


-- Action

type Action
  = Modify Stitches.Action
  | Hold Int
  | Unhold
  | Stitch Position Int
  | Unstitch Position
  | SwitchGrid
  | NoOp


update : Action -> Model -> Model
update action model =
  case action of
    Modify sub ->
      { model | stitches = Stitches.update sub model.stitches }

    Hold id ->
      { model | holding = Just id }

    Unhold ->
      { model | holding = Nothing }

    Stitch position idx ->
      let
        updateBlock block =
          if (fst block) == position then
            position => Just idx
          else
            block
      in
        { model | squares = List.map updateBlock model.squares }

    Unstitch position ->
      let
        updateBlock block =
          if (fst block) == position then
            position => Nothing
          else
            block
      in
        { model | squares = List.map updateBlock model.squares }

    SwitchGrid ->
      { model | hasGrid = not model.hasGrid }

    NoOp ->
      model


view : Address Action -> Model -> Html
view address model =
  let
    w = ((Stitches.count model.stitches) - 1) // 10 * 50 + 550
      |> toString
  in
    div
    [ style
      [ "display" => "flex"
      , "flex-direction" => "row"
      ]
    ]
    [ viewCloth address model
    , gridButton address model
    , viewHoldPanels address model
    , Stitches.view (forwardTo address Modify) model.stitches
    ]


viewCloth : Address Action -> Model -> Html
viewCloth address model =
  elementToHtml
    (List.map fst model.squares)
    (List.map (blockElement address model) model.squares)


elementToHtml : List Position -> List Element -> Html
elementToHtml positions elms =
  let
    w = List.map fst positions
      |> List.foldl max 0

    h = List.map snd positions
      |> List.foldl max 0

    attrs = [ viewBox -2 -2 (w + 4) (h + 4), width 500, height 500 ]
  in
    div
    [ style
      [ "height" => "500px"
      ]
    ]
    [ Svg.svg attrs [ toSvg [] (Group elms) ] ]


blockElement : Address Action -> Model -> (Position, Maybe Int) -> Element
blockElement  address model (pos, maybeStitch) =
  let
    x = fst pos |> toFloat
    y = snd pos |> toFloat

    withCross elm =
      case maybeStitch of
        Just idx ->
          case stitch idx model of
            Just st ->
              Group [ elm, Stitch.element (x + 0.5) (y + 0.5) st ]

            Nothing ->
              Group [ elm ]

        Nothing ->
          Group [ elm ]

    grid elm =
      if model.hasGrid then
        LineStyle Color.black 0.02 elm
          |> DashStyle [ 0.1, 0.1 ] -0.01

      else
        elm

    action =
      case model.holding of
        Just idx ->
          if Maybe.map ((==) idx) maybeStitch ? False then
            Unstitch pos

          else
            Stitch pos idx

        Nothing ->
          Unstitch pos
  in
    Square (x + 0.5) (y + 0.5) 1
      |> Opacity 1.0 0
      |> grid
      |> withCross
      |> Clickable (message address action)



viewHoldPanels : Address Action -> Model -> Html
viewHoldPanels address model =
  let
    w = ((Stitches.count model.stitches) - 1) // 10 * 50 + 50
      |> toString
  in
    div
    [ style
      [ "display" => "flex"
      , "flex-flow" => "column wrap"
      , "height" => "500px"
      , "width" => (w ++ "px")
      ]
    ]
    (toList <| indexedMap (viewHoldPanel address model) model.stitches)


viewHoldPanel : Address Action -> Model -> Int -> Stitch.Model -> Html
viewHoldPanel address model idx stitch =
  (if Maybe.map ((==) idx) model.holding ? False then
    Circle 0 0 0.4
      |> LineStyle (Stitch.color stitch) 0.1
      |> Fill (Stitch.color stitch)
      |> Clickable (message address Unhold)
  else
    Circle 0 0 0.4
      |> LineStyle (Stitch.color stitch) 0.1
      |> Fill Color.white
      |> Clickable (message address (Hold idx))
  )
    |> toHtml 1 1 50 50 []


gridButton : Address Action -> Model -> Html
gridButton address model =
  let
    color =
      case model.hasGrid of
        True ->
          Color.hsl (degrees 100) 0.85 0.85
        False ->
          Color.hsl (degrees 0) 0.85 0.85

  in
    Circle 0 0 0.45
      |> Fill color
      |> Clickable (message address SwitchGrid)
      |> toHtml 1 1 50 50 []