module SewingKit.Pattern
  ( Pattern, cloth, leaf ) where

import Html exposing (..)
import Html.Attributes exposing (style)
import Svg
import Color exposing (Color)
import Signal exposing (Address, message, Message, forwardTo)
import List.Extra exposing (elemIndex)
import Array exposing (indexedMap, toList)
import MultiwayTree exposing (Tree(..))
import MultiwayTreeZipper as Zipper exposing (Zipper)

import SewingKit.Svg exposing (..)
import SewingKit.Stitch as Stitch
import SewingKit.StitchList as StitchList
import SewingKit.Pattern.SquareList as SquareList

(=>) = (,)
(?) = flip Maybe.withDefault
(?>) = Maybe.andThen
(?.) = flip Maybe.map

(!.>) = flip List.map


type alias StitchList =
  StitchList.Model


type alias Stitch =
  Stitch.Model


type alias SquareList =
  SquareList.SquareList StitchId


type alias Square =
  Square.Square StitchId


type alias Model =
  { patternZipper : Zipper Pattern
  , heldStitch : HeldStitch
  , hasGrid : Bool
  }

type Pattern
  = Root SquareList StitchList
  | Node Int SquareList StitchList


type StitchId
  = Id Int
  | ChildId Int StitchId


type HeldStitch
  = HNothing
  | HStitch Int
  --| HStamp Int Stamp


init : Model
init =
  { patternZipper =
    ( Tree ( Root SquareList.init StitchList.init ) [], [] )
  , heldStitch = HStitch 0
  , hasGrid = True
  }


type Action
  = GoToRoot
  | ModifyStitchList StitchList.Action
  | ModifySquareList SquareList.Action
  | HoldStitch Int
  --| HoldStamp Int Stamp
  | Unhold
  | SwitchGrid
  | NoOp


update : Action -> Model -> Model
update action model =
  GoToRoot ->
    { model | patternZipper =
      Zipper.goToRoot model.patternZipper ? model.patternZipper
    }

  ModifyStitchList sub ->
    updateStitchList sub model

  ModifySquareList sub ->
    updateSquareList sub model

  HoldStitch id ->
    { model | heldStitch = HStitch id }

  --HoldStamp id stamp ->
  --  { model | heldStitch = HStamp id stamp }

  Unhold ->
    { model | heldStitch = HNothing }


  SwitchGrid ->
    { model | hasGrid = not model.hasGrid }

  NoOp ->
    model


updateStitchList : StitchList.Action -> Model -> Model
updateStitchList action model =
  let
    updatePattern p =
      case p of
        Root sqrs stchs ->
          Root sqrs (StitchList.update action stchs)

        Node id sqrs stchs ->
          Node id sqrs (StitchList.update action stchs)

  in
    { model | Zipper.updateDatum updatePattern patternZipper }


updateSquareList : SquareList.Action StitchId -> Model -> Model
updateSquareList action model =
  let
    updatePattern p =
      case p of
        Root sqrs stchs ->
          Root (SquareList.update action sqrs) stchs

        Node id sqrs stchs ->
          Node id (SquareList.update action sqrs) stchs

  in
    { model | Zipper.updateDatum updatePattern patternZipper }


-- Query

pattern : Zipper Pattern -> Pattern
pattern zipper =
  Zipper.datum zipper


stitchList : Zipper Pattern -> StitchList
stitchList zipper =
  case pattern zipper of
    Root _ s ->
      s

    Node _ _ s ->
      s


stitchById : StitchId -> Zipper Pattern -> Maybe Stitch
stitchById sid zipper =
  case sid of
    Id id ->
      StitchList.stitch id (stitchList zipper)

    ChildId id sid' ->
      goToChild id zipper
      ?> stitchById sid'


squareList : Zipper Pattern -> SquareList StitchId
squareList zipper =
  case pattern zipper of
    Root s _ ->
      s

    Node _ s _ ->
      s

-- Pattern Zipper

goToRoot : Zipper Pattern -> Zipper Pattern
goToRoot zipper =
  Zipper.goToRoot zipper ? zipper


goToChild : Int -> Zipper Pattern -> Maybe (Zipper Pattern)
goToChild id ((Tree _ children, _) as zipper)=
  elemIndex id children
  ?> (flip Zipper.goToChild) zipper




-- View

-- Square View

viewSquares : Model -> Address SquareList.Action -> SquareList -> Html
viewSquares model address sqrs =
  let
    svg =
      SquareList.positions sqrs
      !.> SquareList.Modify >> forwardTo address
      |> List.map2 (flip << squareElement model) sqrs
      |> Group
      |> MySvg.toSvg []


    attrs =
    [ viewBox
      (SquareList.minX - 2) (SquareList.minY - 2)
      (SquareList.width sqrs + 4) (SquareList.height sqrs + 4)
    , MySvg.width 500
    , MySvg.height 500
    ]
  in
    div
    [ style [] ]
    [ Svg.svg attrs svg ]


squareElement :  Model -> Address (Square.Action StitchId) -> Square -> Element
squareElement model address sqr =
  let
    x = Square.x sqr |> toFloat
    y = Square.y sqr |> toFloat
    root = goToRoot model.patternZipper

    cross elm =
      Square.maybeContent sqr
      ?> (flip stitchById) root
      ?. Stitch.element (x + 0.5) (y + 0.5)
      ?. \sElm -> Group [ elm, sElm ]
      ? Group [ elm ]

    grid elm =
      if model.hasGrid then
        LineStyle Color.black 0.02 elm
          |> DashStyle [ 0.1, 0.1 ] -0.05

      else
        elm

    action =
      case model.heldStitch of
        HStitch id ->
          if Square.maybeContent sqr ?. (!=) (Id id) then
            Square.Modify (Id id)

          else
            Square.Empty

        HNothing ->
          Square.Empty


  in
    Square (x + 0.5) (y + 0.5) 1
    |> Opacity 1.0 0
    |> grid
    |> cross
    |> Clickable (message address action)


-- HeldStitch View



viewHoldPanel : Address Action -> Model -> Html
viewHoldPanel address model =
  let
    sl = stitchList <| goToRoot model.patternZipper
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
