module SewingKit.StitchList
  ( Model, init, color, manner, stitch, count
  , Action, update, view) where

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Color exposing (Color)
import Signal exposing (Address, Message, forwardTo, message)
import Array exposing (Array, push, get, fromList, append, indexedMap, toList, length)
import Array.Extra exposing (sliceFrom, sliceUntil)

import SewingKit.Stitch as Stitch
import SewingKit.Svg as Svg exposing (..)
import Debug exposing (log)

(=>) = (,)

(?) = flip Maybe.withDefault

type alias Model =
  Array Stitch.Model


count : Model -> Int
count =
  length

stitch : Int -> Model -> Maybe Stitch.Model
stitch =
  get


color : Int -> Model -> Maybe Color
color idx model =
  stitch idx model
    |> Maybe.map Stitch.color


manner : Int -> Model -> Maybe Stitch.Manner
manner idx model =
  stitch idx model
    |> Maybe.map Stitch.manner



init : Model
init =
  fromList [ Stitch.init' ]


-- UPDATE

type Action
    = Insert
    | Duplicate Int
    | Remove Int
    | Left Int
    | Right Int
    | Modify Int Stitch.Action


update : Action -> Model -> Model
update action model =
  case action of
    Insert ->
      push Stitch.init' model

    Duplicate idx ->
      push (get idx model ? Stitch.init') model

    Remove idx ->
      append (sliceUntil idx model) (sliceFrom (idx + 1) model)

    Left idx ->
      let
        mLeft = get idx model
        mRight = get (idx - 1) model
        mChanges = Maybe.map2 (\r l -> fromList [ l, r ]) mRight mLeft
      in
        Maybe.map (\cs ->
          sliceUntil (idx - 1) model
          `append` cs
          `append` sliceFrom (idx + 1) model
        ) mChanges
        |> Maybe.withDefault model

    Right idx ->
      let
        mLeft = get (idx + 1) model
        mRight = get idx model
        mChanges = Maybe.map2 (\r l -> fromList [ l, r ]) mRight mLeft
      in
        Maybe.map (\cs ->
          sliceUntil idx model
          `append` cs
          `append` sliceFrom (idx + 2) model
        ) mChanges
        |> Maybe.withDefault model

    Modify idx sub ->
      Array.Extra.update idx (Stitch.update sub) model


-- VIEW

view : Address Action -> Model -> Html
view address model =
  div
  [ style
    [ "display" => "flex"
    , "flex-flow" => "column wrap"
    , "height" => "500px"
    , "align-content" => "flex-start"
    ]
  ]
  ( toList (Array.indexedMap (viewStitch address) model)
  ++ [ insertButton address ]
  )


insertButton : Address Action -> Html
insertButton address =
  div
  [ style
    [ "box-sizing" => "border-box"
    , "width" => "100px"
    , "height" => "75px"
    , "border-color" => "green"
    , "border-style" => "dotted"
    , "display" => "flex"
    , "justify-content" => "space-around"
    , "align-items" => "center"
    ]
  , onClick address Insert
  ]
  [ Cross 0 0 1
    |> LineStyle Color.darkGreen 0.3
    |> LineCap RoundCap
    |> toHtml 1 1 25 25 []
  ]


viewStitch : Address Action -> Int -> Stitch.Model -> Html
viewStitch address idx model =
  let
    context =
      Stitch.Context
        (forwardTo address <| Modify idx)
        (forwardTo address <| always (Duplicate idx))
        (forwardTo address <| always (Remove idx))
        (forwardTo address <| always (Left idx))
        (forwardTo address <| always (Right idx))
  in
    Stitch.viewWithOptions context model