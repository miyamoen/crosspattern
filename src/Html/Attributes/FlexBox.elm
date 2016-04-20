module Html.FlexBox (Style) where

import Html exposing (..)
import Html.Attributes exposing (style)


(=>) = (,)

type Direction
  = Row
  | RowReverse
  | Column
  | ColumnReverse


direction : Direction -> String
direction dir =
  case dir of
    Row ->
      "row"

    RowReverse ->
      "row-reverse"

    Column ->
      "column"

    ColumnReverse ->
      "column-reverse"


type Wrap
  = NoWrap
  | Wrap
  | WrapReverse


toStringFromWrap : Wrap -> String
toStringFromWrap wrap =
  case wrap ->
    NoWrap ->
      "nowrap"

    Wrap ->
      "wrap"

    WrapReverse ->
      "wrap-reverse"


noWrap : Wrap
noWrap =
  NoWrap

wrap : Wrap
wrap =
  Wrap


wrapReverse : Wrap
wrapReverse =
  WrapReverse


type JustifyContent
  = JCStart
  | JCEnd
  | JCCenter
  | JCSpaceBetween
  | JCSpaceAround


toStringFromJustifyContent : JustifyContent -> String
toStringFromJustifyContent jc =
  case jc of
    JCStart ->
      "flex-start"

    JCEnd ->
      "flex-end"

    JCCenter ->
      "center"

    JCSpaceBetween ->
      "space-between"

    JCSpaceAround ->
      "space-around"


type AlignItems
  = AIStretch
  | AIStart
  | AIEnd
  | AICenter
  | AIBaseline


toStringFromAlignItems : AlignItems -> String
toStringFromAlignItems alignItems =
  case alignItems of
    AIStretch ->
      "stretch"

    AIStart ->
      "flex-start"

    AIEnd ->
      "flex-end"

    AICenter ->
      "center"

    AIBaseline ->
      "baseline"


type AlignContent
  = ACStretch
  | ACStart
  | ACEnd
  | ACCenter
  | ACSpaceBetween
  | ACSpaceAround


toStringFromAlignContent : AlignContent -> String
toStringFromAlignContent alignContent =
  case alignContent of
    ACStretch ->
      "stretch"

    ACStart ->
      "flex-start"

    ACEnd ->
      "flex-end"

    ACCenter ->
      "center"

    ACSpaceBetween ->
      "space-between"

    ACSpaceAround ->
      "space-around"


type alias ContainerStyle =
  { direction : Direction
  , wrap : Wrap
  , justifyContent : JustifyContent
  , alignItems : AlignItems
  , alignContent : AlignContent
  }


defaultContainerStyle : ContainerStyle
defaultContainerStyle =
  { direction = Row
  , wrap = NoWrap
  , justifyContent = JCStart
  , alignItems = AIStretch
  , alignContent = ACStretch
  }


toCssFromContainerStye : ContainerStyle -> Css
toCssFromContainerStye cStyle =
  [ "flex-direction" => toStringFromDirection cStyle.direction
  , "flex-wrap" => toStringFromWrap cStyle.wrap
  , "justify-content" => toStringFromJustifyContent cStyle.justifyContent
  , "align-items" => toStringFromAlignItems cStyle.alignItems
  , "align-content" => toStringFromAlignContent cStyle.alignContent
  ]


type alias Order =
  Int

type alias Grow =
  Float

type alias Shrink =
  Float

type alias Basis =
  Float

type alias AlignSelf =
  AlignItems