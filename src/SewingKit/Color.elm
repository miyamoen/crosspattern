module SewingKit.Color
  ( Model(..), HSL(..)
  , color, state, fromColor, toRecord, init, init', toString'
  , Action, view, update
  ) where

import Signal exposing (Address, Message, forwardTo, message)
import Html exposing (..)
import Html.Attributes as Attributes exposing (style)
import Html.Events exposing (on, targetValue, onClick)
import String
import Color exposing (Color)
import SewingKit.Svg exposing (..)
import List.Extra exposing (lift2, zip)


(=>) = (,)

----------Model----------
type Model =
  Poped Color | Unpoped Color


type HSL =
  Hue | Saturation | Lightness


color : Model -> Color
color model =
  case model of
    Poped clr ->
      clr

    Unpoped clr ->
      clr


state : Model -> (Color -> Model)
state model =
  case model of
    Poped _ ->
      Poped

    Unpoped _ ->
      Unpoped


fromColor : Color -> Model
fromColor color =
  Unpoped color


toRecord : Model -> { hue : Float, saturation : Float, lightness : Float }
toRecord model =
  let
    r = color model |> Color.toHsl
  in
    { hue = r.hue, saturation = r.saturation, lightness = r.lightness }


init : Float -> Float -> Float -> Model
init h s l =
  Color.hsl h s l |> Unpoped


init' : Model
init' =
  init (degrees 350) 0.85 0.85


toString' : Model -> String
toString' =
  SewingKit.Svg.toCss << color


----------Action----------
type Action
  = Input Float Float Float
  | InputColor Color
  | Pop
  | Unpop
  | NoOp


----------Update----------
update : Action -> Model -> Model
update action model =
  let
    hsl = toRecord model
  in
    case action of
      Input h s l ->
        Color.hsl h s l |> state model

      InputColor clr ->
        (state model) clr

      Pop ->
        Poped <| color model

      Unpop ->
        Unpoped <| color model

      NoOp ->
        model


----------View----------

view : Address Action -> Model -> Html
view address model =
  div
  [ style
    [ "height" => "50px"
    ]
  ]
  (case model of
    Unpoped clr ->
    [ Circle 0 0 0.45
      |> Fill clr
      |> Clickable (message address Pop)
      |> toHtml 1 1 50 50 []
    ]

    Poped clr ->
    [ Circle 0 0 0.45
      |> Fill clr
      |> Clickable (message address Pop)
      |> toHtml 1 1 50 50 []
    , pickerAndInput address clr
    , opaqueLayer address
    ]
  )


opaqueLayer : Address Action -> Html
opaqueLayer address =
  div
  [ onClick address Unpop
  , style
    [ "background-color" => "black"
    , "position" => "fixed"
    , "top" => "0"
    , "left" => "0"
    , "z-index" => "1"
    , "width" => "100vw"
    , "height" => "100vh"
    , "opacity" => "0.3"
    ]
  ]
  []


pickerAndInput : Address Action -> Color -> Html
pickerAndInput address color =
  div
  [ style
    [ "position" => "fixed"
    , "top" => "calc(50vh - 100px)"
    , "left" => "calc(50vw - 200px)"
    , "z-index" => "3"
    , "display" => "flex"
    , "background-color" => "white"
    ]
  ]
  [ picker address color
  , console address color
  ]


console : Address Action -> Color -> Html
console address clr =
  div
  [ style
    [ "display" => "flex"
    , "flex-direction" => "column"
    , "align-items" => "flex-end"
    ]
  ]
  [ inputs address clr
  , consolePanel address clr
  ]


consolePanel : Address Action -> Color -> Html
consolePanel address clr =
  div[][]


inputs : Address Action -> Color -> Html
inputs address clr =
  div
  [ style
    [ "display" => "flex"
    , "flex-direction" => "column"
    , "align-items" => "flex-end"
    ]
  ]
  [ input address Hue clr
  , input address Saturation clr
  , input address Lightness clr
  ]


input : Address Action -> HSL -> Color -> Html
input address hsl clr =
  let
    val =
      case hsl of
        Hue ->
          Color.toHsl clr |> .hue |> toDegrees

        Saturation ->
          Color.toHsl clr |> .saturation

        Lightness ->
          Color.toHsl clr |> .lightness

  in
    div
    [ style
      [ "display" => "flex"
      ]
    ]
    [ label
      [ style
        [ "display" => "flex"
        , "justify-content" => "flex-end"
        , "align-items" => "center"
        , "padding" => "5px"
        ]
      ]
      [ text <| toString hsl ]
    , Html.input
      [ toString val |> String.left 5 |> Attributes.value
      , onBlur address hsl clr
      , inputStyle
      ] []
    ]


inputStyle : Attribute
inputStyle =
  style
  [ "box-sizing" => "border-box"
  , "width" => "70px"
  , "height" => "50px"
  , "text-align" => "left"
  , "border" => "1px solid black"
  , "margin" => "0"
  , "padding" => "5px"
  ]


onBlur : Address Action -> HSL -> Color -> Attribute
onBlur address hsl clr =
  let
    r = Color.toHsl clr
    actionByFloat float =
      case hsl of
        Hue ->
          Input (degrees float) r.saturation r.lightness

        Saturation ->
          Input r.hue float r.lightness

        Lightness ->
          Input r.hue r.saturation float

    actionByString
      = String.toFloat
      >> Result.map actionByFloat
      >> Result.withDefault NoOp
  in
    on "blur" targetValue ( actionByString >> message address )





----------Svg Element----------

picker : Address Action -> Color -> Html
picker address clr =
  Group
  [ circleByHue address clr
  , squareBySL address clr
  ]
  |> toHtml 20 20 200 200 []


squareBySL : Address Action -> Color -> Element
squareBySL address clr =
  let
    h = Color.toHsl clr |> .hue
    steps = 10
    w = 10
    value = coords steps 1 0.5
    vxs = zip value (coords steps w 0)
    vys  = zip (List.reverse value) (coords steps w 0)
  in
    lift2
      (\(s, x) (l, y) ->
        square address (Color.hsl h s l) x y (w / steps)
      )
      vxs vys
      |> Group


square : Address Action -> Color -> Float -> Float -> Float -> Element
square address clr x y w =
  Square x y w
    |> Fill clr
    |> Clickable (message address <| InputColor clr)


circleByHue : Address Action -> Color -> Element
circleByHue address clr =
  let
    steps = 24
    w = 360 / steps -- 15
    out = 9.5
    in' = 7.5

    sectors = range 0 360 w
      |> List.map (\s -> sector address clr out s (s + w))

    inside = Circle 0 0 in'
      |> Fill (Color.hsl 0 1.0 1.0)
  in
    Group (sectors ++ [ inside ])


sector : Address Action -> Color -> Float -> Float -> Float -> Element
sector address clr r s e =
  let
    rec = Color.toHsl clr
  in
    Sector 0 0 r s e
      |> Fill (Color.hsl (degrees s) 0.8 0.7)
      |> Clickable (message address
          <| Input (degrees s) rec.saturation rec.lightness
        )



toDegrees : Float -> Float
toDegrees rad =
  rad * 180 / pi


range : Float -> Float -> Float -> List Float
range start stop step =
  if start <= stop then
    stop :: range start (stop - step) step
  else
    []


coords : Int -> Float -> Float -> List Float
coords steps width center =
  let
    w = width / (toFloat steps)
    l = center - width / 2 + w / 2
    r = center + width / 2 - w / 2
  in
    range l r w
