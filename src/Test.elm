import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue, keyCode)
import Signal exposing (Address)
import StartApp.Simple as StartApp
import String exposing (toFloat)
(=>) = (,)

main =
  div
  [ style
    [ "margin-top" => "120px"
    , "display" => "flex"
    , "justify-content" => "center"
    ]
  ]
  [ myHeader
  , loginPanel
  ]

myHeader : Html
myHeader =
  header
  [ style
    [ "position" => "fixed"
    , "top" => "0"
    , "left" => "0"
    , "height" => "70px"
    , "width" => "100%"
    , "background-color" => "grey"
    , "display" => "flex"
    ]
  ]
  [ a [ style ["margin-right" => "auto"]] [ text "Logo" ]
  , button [] [ text "Login" ]
  , button [] [ text "Registration" ]
  ]

loginPanel : Html
loginPanel =
  div
  [ style
    [ "display" => "flex"
    , "justify-content" => "space-around"
    , "align-items" => "center"
    , "width" => "300px"
    , "height" => "300px"
    , "background-color" => "gray"
    ]
  ]
  [ button [] [ text "Login" ]
  , button [] [ text "Registration" ]
  ]