module SewingKit.Svg
  ( toCss, svgOnCenter
  , viewBox, width, height
  , Element(..), toSvg, toHtml
  , sector, circle, square, line, slash, backSlash, xCross, cross
  , Cap(..), Join(..)
  ) where

import Color exposing (Color)
import Svg exposing (Svg, Attribute, path, rect, polyline, g)
import Svg.Attributes as Attributes exposing ( d, x, y, r, points
  , strokeLinecap, strokeLinejoin, strokeMiterlimit
  , stroke, strokeWidth, fill, transform
  , strokeOpacity, fillOpacity
  , strokeDasharray, strokeDashoffset)
import Svg.Events exposing (onClick)
import Html exposing (Html)
import String
import Native.MySvg

(=>) = (,)

toCss : Color -> String
toCss =
  Native.MySvg.toCss


---- Attributes

viewBox : Float -> Float -> Float -> Float -> Attribute
viewBox x y w h =
  String.join " " [ toString x, toString y, toString w, toString h ]
    |> Attributes.viewBox


width : Float -> Attribute
width w =
  Attributes.width (toString w ++ "px")


height : Float -> Attribute
height h =
  Attributes.height (toString h ++ "px")


svgOnCenter : Float -> Float -> Float -> Float -> List Html.Attribute -> List Svg -> Html
svgOnCenter vw vh w h attributes svgs =
  Svg.svg
  ([ viewBox (vw / -2) (vh / -2) vw vh
  , width w
  , height h
  ] ++ attributes )
  svgs


-- Element

type Element
  = Sector Float Float Float Float Float
  | Circle Float Float Float
  | Square Float Float Float
  | Line (List (Float, Float))
  | Slash Float Float Float
  | BackSlash Float Float Float
  | XCross Float Float Float
  | Cross Float Float Float
  | Grid Float Int Int Float Float
  | Polygon (List (Float, Float))
  | Clickable Signal.Message Element
  | Fill Color Element
  | LineCap Cap Element
  | LineJoin Join Element
  | LineStyle Color Float Element
  | DashStyle (List Float) Float Element
  | Opacity Float Float Element
  | Move Float Float Element
  | Group (List Element)


toHtml : Float -> Float -> Float -> Float -> List Html.Attribute -> Element -> Html
toHtml vw vh w h attributes elm =
  [ toSvg [] elm ] |> svgOnCenter vw vh w h attributes


toSvg : List Attribute -> Element -> Svg
toSvg attributes elm =
  case elm of
    Sector x y r s e ->
      sector x y r s e attributes

    Circle x y r ->
      circle x y r attributes

    Square x y w ->
      square x y w attributes

    Line points ->
      line points attributes

    Slash x y w ->
      slash x y w attributes

    BackSlash x y w ->
      backSlash x y w attributes

    XCross x y w ->
      xCross x y w attributes

    Cross x y w ->
      cross x y w attributes

    Grid stride w h x y ->
      grid stride w h x y attributes

    Polygon points ->
      polygon points attributes

    Clickable message elm ->
      toSvg (onClick message :: attributes) elm

    Fill color elm ->
      toSvg (Attributes.fill (toCss color) :: attributes) elm

    LineCap cap elm ->
      toSvg (lineCap cap ++ attributes ) elm

    LineJoin join elm ->
      toSvg (lineJoin join ++ attributes) elm

    LineStyle c w elm ->
      toSvg (lineStyle c w ++ attributes) elm

    DashStyle p o elm ->
      toSvg (dashStyle p o ++ attributes) elm

    Opacity stroke fill elm ->
      toSvg (opacity stroke fill ++ attributes) elm

    Move tx ty elm ->
      toSvg (move tx ty :: attributes) elm

    Group elms ->
      List.map (toSvg []) elms
        |> g attributes


-- Alias & Svg function

---- Sector

sector : Float -> Float -> Float -> Float -> Float -> List Attribute -> Svg
sector cx cy radius start end attributes =
  let
    toX = \deg ->      radius * sin (degrees deg) + cx |> toString
    toY = \deg -> -1 * radius * cos (degrees deg) + cy |> toString

    p = d (String.join " "
      [ "M", toString cx, toString cy
      , "L", toX start, toY start
      , "A", toString radius, toString radius
      , toString (start - 90), "0 1"
      , toX end, toY end
      , "z"
      ])
  in
    path (p :: attributes) []


---- Circle

circle : Float -> Float -> Float -> List Attribute -> Svg
circle cx cy radius attributes =
  let attrs =
    [ Attributes.cx (toString cx)
    , Attributes.cy (toString cy)
    , r (toString radius)
    ]
  in
    Svg.circle ( attrs ++ attributes ) []


---- Square

square : Float -> Float -> Float -> List Attribute -> Svg
square cx cy w attributes =
  let attrs =
    [ x (cx - w / 2 |> toString)
    , y (cy - w / 2 |> toString)
    , width w
    , height w
    ]
  in
    rect ( attrs ++ attributes ) []


---- Line

line : List (Float, Float) -> List Attribute -> Svg
line points attributes =
  let
    attr = points
      |> List.map (\(x, y) -> (toString x) ++ "," ++ (toString y))
      |> String.join " "
      |> Attributes.points
  in
    polyline ( attr :: attributes ) []


---- Slash

slash : Float -> Float -> Float -> List Attribute -> Svg
slash cx cy width attributes =
  line
    [ (cx - width / 2) => (cy + width / 2)
    , (cx + width / 2) => (cy - width / 2)
    ]
    attributes


---- Back Slash

backSlash : Float -> Float -> Float -> List Attribute -> Svg
backSlash cx cy width attributes =
  line
    [ (cx - width / 2) => (cy - width / 2)
    , (cx + width / 2) => (cy + width / 2)
    ]
    attributes


---- X Cross

xCross : Float -> Float -> Float -> List Attribute -> Svg
xCross cx cy width attributes =
  let
    slash' = slash cx cy width attributes
    backSlash' = backSlash cx cy width attributes
  in
    g [] [ slash', backSlash' ]


---- Cross

cross : Float -> Float -> Float -> List Attribute -> Svg
cross cx cy width attributes =
  let
    vLine = [ cx => (cy - width / 2), cx => (cy + width / 2) ]
    hLine = [ (cx - width / 2) => cy, (cx + width / 2) => cy ]
  in
    g [] [ line vLine attributes, line hLine attributes ]


---- Grid

grid : Float -> Int -> Int -> Float -> Float -> List Attribute -> Svg
grid stride w h x y attributes =
  let
    height = toString <| y + stride * (toFloat h)
    vs = List.map (\dx -> (toFloat dx) * stride + x) [0..w]
      |> List.map toString
      |> List.map (\x' -> "M " ++ x' ++ " 0, v " ++ height)

    width = toString <| x + stride * (toFloat w)
    hs = List.map (\dy -> (toFloat dy) * stride + y) [0..h]
      |> List.map toString
      |> List.map (\y' -> "M 0 " ++ y' ++ ", h " ++ width)

  in
    path (d (String.join " " (vs ++ hs)) :: attributes) []


---- Polygon

polygon : List (Float, Float) -> List Attribute -> Svg
polygon points attributes =
  let
    attr = points
      |> List.map (\(x, y) -> (toString x) ++ "," ++ (toString y))
      |> String.join " "
      |> Attributes.points
  in
    Svg.polygon ( attr :: attributes ) []
--------------------------------------------



type Cap = ButtCap | RoundCap | SquareCap

lineCap : Cap -> List Attribute
lineCap cap =
  let
    param = case cap of
      ButtCap -> "butt"
      RoundCap -> "round"
      SquareCap -> "square"
  in
    [ strokeLinecap param ]


type Join = MiterJoin Float | RoundJoin | BevelJoin


lineJoin : Join -> List Attribute
lineJoin join =
  case join of
    MiterJoin limit ->
      [ strokeLinejoin "miter", strokeMiterlimit <| toString limit ]

    RoundJoin ->
      [ strokeLinejoin "round" ]

    BevelJoin ->
      [ strokeLinejoin "bevel" ]


lineStyle : Color -> Float -> List Attribute
lineStyle c w =
  [ stroke <| toCss c, strokeWidth <| toString w ]


dashStyle : List Float -> Float -> List Attribute
dashStyle pattern offset =
  let
    ps = List.map toString pattern
      |> String.join " "
  in
    [ strokeDashoffset <| toString offset, strokeDasharray ps ]

opacity : Float -> Float -> List Attribute
opacity stroke fill =
  [ strokeOpacity <| toString stroke, fillOpacity <| toString fill ]


move : Float -> Float -> Attribute
move tx ty =
  String.concat [ "translate(", toString tx, ",", toString ty, ")" ]
    |> transform