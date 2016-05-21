module SewingKit.Svg exposing
  ( svgOnCenter
  , viewBox, width, height
  , Element(..), toSvg, toHtml
  , sector, circle, square, line, slash, backSlash, xCross, cross
  , Cap(..), Join(..)
  )

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
import Color.Convert exposing (colorToCssHsl)

(=>) = (,)

---- Attributes

viewBox : Float -> Float -> Float -> Float -> Attribute msg
viewBox x y w h =
  String.join " " [ toString x, toString y, toString w, toString h ]
    |> Attributes.viewBox


width : Float -> Attribute msg
width w =
  Attributes.width (toString w ++ "px")


height : Float -> Attribute msg
height h =
  Attributes.height (toString h ++ "px")


svgOnCenter : Float -> Float -> List (Html.Attribute msg) -> List (Svg msg) -> Html msg
svgOnCenter vw vh attributes svgs =
  Svg.svg
  ( viewBox (vw / -2) (vh / -2) vw vh :: attributes )
  svgs


-- Element

type Element msg
  = Sector Point Point Length Degree Degree
  | Circle Point Point Length
  | Square Point Point Length
  | Line (List (Point, Point))
  | Slash Point Point Length
  | BackSlash Point Point Length
  | XCross Point Point Length
  | Cross Point Point Length
  | Grid Length Count Count Point Point
  | Polygon (List (Point, Point))
  | Clickable msg (Element msg)
  | Fill Color (Element msg)
  | LineCap Cap (Element msg)
  | LineJoin Join (Element msg)
  | LineStyle Color Thickness (Element msg)
  | DashStyle (List Length) Offset (Element msg)
  | Opacity Float Float (Element msg)
  | Group (List (Element msg))


type alias Point =
  Float
  
  
type alias Length =
  Float
  
type alias Offset =
  Float

  
type alias Degree =
  Float
  

type alias Count =
  Int
  
type alias Step =
  Float
  

type alias Thickness =
  Float


toHtml : Float -> Float -> List (Attribute msg) -> Element msg -> Html msg
toHtml vw vh attributes elm =
  svgOnCenter vw vh attributes [ toSvg [] elm ]


toSvg : List (Attribute msg) -> Element msg -> Svg msg
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

    Grid step w h x y ->
      grid step w h x y attributes

    Polygon points ->
      polygon points attributes

    Clickable message elm ->
      toSvg (onClick message :: attributes) elm

    Fill color elm ->
      toSvg (Attributes.fill (colorToCssHsl color) :: attributes) elm

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

    Group elms ->
      List.map (toSvg []) elms
        |> g attributes


-- Alias & Svg function

---- Sector

sector : Point -> Point -> Length -> Degree -> Degree -> List (Attribute msg) -> Svg msg
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

circle : Point -> Point -> Length -> List (Attribute msg) -> Svg msg
circle cx cy radius attributes =
  let attrs =
    [ Attributes.cx (toString cx)
    , Attributes.cy (toString cy)
    , r (toString radius)
    ]
  in
    Svg.circle ( attrs ++ attributes ) []


---- Square

square : Point -> Point -> Length -> List (Attribute msg) -> Svg msg
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

line : List (Point, Point) -> List (Attribute msg) -> Svg msg
line points attributes =
  let
    attr = points
      |> List.map (\(x, y) -> (toString x) ++ "," ++ (toString y))
      |> String.join " "
      |> Attributes.points
  in
    polyline ( attr :: attributes ) []


---- Slash

slash : Point -> Point -> Length -> List (Attribute msg) -> Svg msg
slash cx cy width attributes =
  line
    [ (cx - width / 2) => (cy + width / 2)
    , (cx + width / 2) => (cy - width / 2)
    ]
    attributes


---- Back Slash

backSlash : Point -> Point -> Length -> List (Attribute msg) -> Svg msg
backSlash cx cy width attributes =
  line
    [ (cx - width / 2) => (cy - width / 2)
    , (cx + width / 2) => (cy + width / 2)
    ]
    attributes


---- X Cross

xCross : Point -> Point -> Length -> List (Attribute msg) -> Svg msg
xCross cx cy width attributes =
  let
    slash' = slash cx cy width attributes
    backSlash' = backSlash cx cy width attributes
  in
    g [] [ slash', backSlash' ]


---- Cross

cross : Point -> Point -> Length -> List (Attribute msg) -> Svg msg
cross cx cy width attributes =
  let
    vLine = [ cx => (cy - width / 2), cx => (cy + width / 2) ]
    hLine = [ (cx - width / 2) => cy, (cx + width / 2) => cy ]
  in
    g [] [ line vLine attributes, line hLine attributes ]


---- Grid

grid : Length -> Count -> Count -> Point -> Point -> List (Attribute msg) -> Svg msg
grid step xCount yCount x y attributes =
  let
    xy = toString x ++ ", " ++ toString y
    height = step * (toFloat yCount)
    width = step * (toFloat xCount)
    
    vlines =
      (++) ("M " ++ xy ++ "　v " ++ toString height)
      <| String.repeat xCount
      <| " m " ++ toString step ++ ", " ++ toString -height
   
    
    hlines =
      (++) ("M " ++ xy ++ "　h " ++ toString width)
      <| String.repeat yCount
      <| " m " ++ toString -width ++ ", " ++ toString step
  in
    path (d ( vlines ++ " " ++ hlines) :: attributes) []


---- Polygon

polygon : List (Point, Point) -> List (Attribute msg) -> Svg msg
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

lineCap : Cap -> List (Attribute msg)
lineCap cap =
  let
    param = case cap of
      ButtCap -> "butt"
      RoundCap -> "round"
      SquareCap -> "square"
  in
    [ strokeLinecap param ]


type Join = MiterJoin Float | RoundJoin | BevelJoin


lineJoin : Join -> List (Attribute msg)
lineJoin join =
  case join of
    MiterJoin limit ->
      [ strokeLinejoin "miter", strokeMiterlimit <| toString limit ]

    RoundJoin ->
      [ strokeLinejoin "round" ]

    BevelJoin ->
      [ strokeLinejoin "bevel" ]


lineStyle : Color -> Thickness -> List (Attribute msg)
lineStyle c w =
  [ stroke <| colorToCssHsl c, strokeWidth <| toString w ]


dashStyle : List Length -> Offset -> List (Attribute msg)
dashStyle pattern offset =
  let
    ps = List.map toString pattern
      |> String.join " "
  in
    [ strokeDashoffset <| toString offset, strokeDasharray ps ]

opacity : Float -> Float -> List (Attribute msg)
opacity stroke fill =
  [ strokeOpacity <| toString stroke, fillOpacity <| toString fill ]


