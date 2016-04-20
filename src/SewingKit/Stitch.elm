module SewingKit.Stitch
  ( Model, init, init', manner, state, color
  , Manner(..), State(..)
  , Action, update
  , view, Context, viewWithOptions, element) where

import Html exposing (..)
import Html.Attributes exposing (style)
import Color exposing (Color)
import Signal exposing (Address, Message, forwardTo, message)

import SewingKit.Color as SColor
import SewingKit.Svg as Svg exposing (..)

(=>) = (,)

type alias Model =
  { manner : Manner
  , state : State
  , color : SColor.Model
  }


type Manner
  = Cross
  | Slash
  | BackSlash


type State
  = Folded | Dropped


init' : Model
init' =
  { manner = Cross
  , color = SColor.init'
  , state = Folded
  }


init : Float -> Float -> Float -> Model
init h s l =
  { manner = Cross
  , color = SColor.init h s l
  , state = Folded
  }


color : Model -> Color
color model =
  SColor.color model.color


state : Model -> State
state =
  .state


manner : Model -> Manner
manner =
  .manner



type Action
  = Stitch Manner
  | Drop
  | Fold Manner
  | ModifyColor SColor.Action
  | NoOp


update : Action -> Model -> Model
update action model =
  case action of
    Stitch manner ->
      { model | manner = manner }

    Drop ->
      { model | state = Dropped }

    Fold manner ->
      { model | state = Folded, manner = manner }

    ModifyColor sub ->
      { model | color = SColor.update sub model.color }

    NoOp ->
      model



view : Address Action -> Model -> Html
view address model =
  div
  [ style
    [ "position" => "relative"
    , "display" => "flex"
    , "flex-direction" => "row"
    ]
  ]
  (case (state model) of
    Folded ->
    [ element 0 0 model
      |> Clickable (message address Drop)
      |> toHtml 1 1 50 50 []
    , SColor.view (forwardTo address ModifyColor) model.color
    ]

    Dropped ->
    [ element 0 0 model
      |> Clickable (message address <| Fold model.manner )
      |> toHtml 1 1 50 50 []
    , viewDropped address model
    , SColor.view (forwardTo address ModifyColor) model.color
    ]
  )


viewDropped : Address Action -> Model -> Html
viewDropped address model =
  div
  [ style
    [ "background-color" => "white"
    , "position" => "absolute"
    , "top" => "50px"
    , "left" => "0px"
    , "z-index" => "1"
    ]
  ]
  ([ Cross, Slash, BackSlash ]
    |> List.filter ((/=) model.manner)
    |> List.map (\manner -> { model | manner = manner })
    |> List.map
      (\m -> element 0 0 m
      |> Clickable (message address <| Fold m.manner)
      |> toHtml 1 1 50 50 []
      )
  )


element : Float -> Float -> Model -> Element
element x y model =
  let
    len = 0.68
    elm = case manner model of
      Cross ->
        XCross x y len

      Slash ->
        Svg.Slash x y len

      BackSlash ->
        Svg.BackSlash x y len
  in
    elm
      |> LineStyle (color model) 0.3
      |> LineCap RoundCap


type alias Context =
  { actions : Address Action
  , duplicate : Address ()
  , remove : Address ()
  , left : Address ()
  , right : Address ()
  }


viewWithOptions : Context -> Model -> Html
viewWithOptions context model =
  div
  [ style
    [ "display" => "flex"
    , "flex-direction" => "column"
    ]
  ]
  [ view context.actions model
  , div
    [ style [ "display" => "flex" ] ]
    [ Polygon [ 0 => -0.45, 0.45 => 0.45, -0.45 => 0.45 ]
      |> Fill Color.darkBlue
      |> Clickable (Signal.message context.left ())
      |> toHtml 1 1 25 25 []

    , Line [ -0.45 => 0, 0.45 => 0 ]
      |> LineStyle Color.red 0.3
      |> LineCap RoundCap
      |> Clickable (Signal.message context.remove ())
      |> toHtml 1 1 25 25 []

    , Svg.Cross 0 0 0.95
      |> LineStyle Color.darkGreen 0.3
      |> LineCap ButtCap
      |> Clickable (Signal.message context.duplicate ())
      |> toHtml 1 1 25 25 []

    , Polygon [ 0 => 0.45, -0.45 => -0.45, 0.45 => -0.45 ]
      |> Fill Color.darkBlue
      |> Clickable (Signal.message context.right ())
      |> toHtml 1 1 25 25 []
    ]
  ]