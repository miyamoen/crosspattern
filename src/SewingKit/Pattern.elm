module SewingKit.Pattern where

import Html exposing (..)
import Html.Attributes exposing (style)
import Svg
import Color exposing (Color)
import Signal exposing (Address, message, Message, forwardTo)
import List.Extra exposing (elemIndex)
import Array exposing (indexedMap, toList)
import MultiwayTree exposing (Tree(..))
import MultiwayTreeZipper as Zipper exposing (Zipper)

import SewingKit.Svg as MySvg exposing (..)
import SewingKit.Stitch as Stitch
import SewingKit.StitchList as StitchList
import SewingKit.Pattern.MeshList as MeshList
import SewingKit.Pattern.Mesh as Mesh

(=>) = (,)
(?) = flip Maybe.withDefault
(?>) = Maybe.andThen
(?.) = flip Maybe.map



type alias StitchList =
  StitchList.Model


type alias Stitch =
  Stitch.Model


type alias MeshList =
  MeshList.MeshList StitchId


type alias Mesh =
  Mesh.Mesh StitchId


type alias Model =
  { patternZipper : Zipper Pattern
  , holding : Holding
  , hasGrid : Bool
  }

type Pattern
  = Root MeshList StitchList
  | Node Int MeshList StitchList


type StitchId
  = Id Int
  | ChildId Int StitchId


type Holding
  = HNothing
  | HStitch Int
  --| HStamp Int Stamp


init : Model
init =
  { patternZipper =
    ( Tree ( Root MeshList.init StitchList.init ) [], [] )
  , holding = HStitch 0
  , hasGrid = True
  }


type Action
  = GoToRoot
  | ModifyStitchList StitchList.Action
  | ModifyMeshList (MeshList.Action StitchId)
  | HoldStitch Int
  --| HoldStamp Int Stamp
  | Unhold
  | SwitchGrid
  | NoOp


update : Action -> Model -> Model
update action model =
  case action of
    GoToRoot ->
      { model | patternZipper = goToRoot model.patternZipper }

    ModifyStitchList sub ->
      updateStitchList sub model

    ModifyMeshList sub ->
      updateMeshList sub model

    HoldStitch id ->
      { model | holding = HStitch id }

    --HoldStamp id stamp ->
    --  { model | holding = HStamp id stamp }

    Unhold ->
      { model | holding = HNothing }


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
    { model | patternZipper
      = Zipper.updateDatum updatePattern model.patternZipper
      ? model.patternZipper
    }


updateMeshList : MeshList.Action StitchId -> Model -> Model
updateMeshList action model =
  let
    updatePattern p =
      case p of
        Root sqrs stchs ->
          Root (MeshList.update action sqrs) stchs

        Node id sqrs stchs ->
          Node id (MeshList.update action sqrs) stchs

  in
    { model | patternZipper
      = Zipper.updateDatum updatePattern model.patternZipper
      ? model.patternZipper
    }


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

    --_ ->
    --  Nothing
    ChildId id sid' ->
      goToChild id zipper
      ?> stitchById sid'


meshList : Zipper Pattern -> MeshList
meshList zipper =
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
goToChild id ((Tree _ children, _) as zipper) =
  let
    f (Tree pattern _) =
      case pattern of
        Node id _ _ ->
          Just id

        _ ->
          Nothing
  in
    (List.map f children
    |> elemIndex (Just id))
    ?> (flip Zipper.goToChild) zipper




-- View

view : Address Action -> Model -> Html
view address model =
  div
  [ style
    [ "display" => "flex"
    , "flex-direction" => "row"
    ]
  ]
  [ viewMeshs model
    (forwardTo address ModifyMeshList)
    (meshList <| goToRoot model.patternZipper)
  , gridButton address model
  , holdPanel address model
  , StitchList.view
    (forwardTo address ModifyStitchList)
    (stitchList <| goToRoot model.patternZipper)
  ]



-- Mesh View

viewMeshs : Model -> Address (MeshList.Action StitchId) -> MeshList -> Html
viewMeshs model address sqrs =
  let
    svg =
      MeshList.positions sqrs
      |> List.map (forwardTo address << MeshList.Modify)
      |> List.map2 (flip <| meshElement model) sqrs
      |> Group
      |> MySvg.toSvg []


    attrs =
    [ viewBox
      ((MeshList.minX sqrs ? 0) - 2 |> toFloat)
      ((MeshList.minY sqrs ? 0) - 2 |> toFloat)
      (MeshList.width sqrs + 4 |> toFloat)
      (MeshList.height sqrs + 4 |> toFloat)
    , MySvg.width 500
    , MySvg.height 500
    ]
  in
    div
    [ style [] ]
    [ Svg.svg attrs [ svg ] ]


meshElement : Model -> Address (Mesh.Action StitchId) -> Mesh -> Element
meshElement model address sqr =
  let
    x = Mesh.x sqr |> toFloat
    y = Mesh.y sqr |> toFloat
    root = goToRoot model.patternZipper

    cross elm =
      Mesh.maybeContent sqr
      ?> (flip stitchById) root
      ?. Stitch.element (x + 0.5) (y + 0.5)
      ?. (\sElm -> Group [ elm, sElm ])
      ? Group [ elm ]

    grid elm =
      if model.hasGrid then
        LineStyle Color.black 0.02 elm
          |> DashStyle [ 0.1, 0.1 ] -0.05

      else
        elm

    action =
      case model.holding of
        HStitch id ->
          if Mesh.maybeContent sqr ?. (/=) (Id id) ? True then
            Mesh.Modify (Id id)

          else
            Mesh.Empty

        HNothing ->
          Mesh.Empty


  in
    Mesh (x + 0.5) (y + 0.5) 1
    |> Opacity 1.0 0
    |> grid
    |> cross
    |> Clickable (message address action)


-- Holding View


holdPanel : Address Action -> Model -> Html
holdPanel address model =
  let
    stitches = stitchList <| goToRoot model.patternZipper

  in
    div
    [ style
      [ "display" => "flex"
      , "flex-flow" => "column wrap"
      , "height" => "500px"
      --, "width" => (w ++ "px")
      ]
    ]
    (indexedMap (holdPanelButton address model) stitches |> toList)


holdPanelButton : Address Action -> Model -> Int -> Stitch -> Html
holdPanelButton address model id stitch =
  let
    fillAndAction =
      case model.holding of
        HStitch hId ->
          if id == hId
            then Fill (Stitch.color stitch)
            >> Clickable (message address Unhold)

            else Fill Color.white
            >> Clickable (message address <| HoldStitch id)

        _ ->
          Fill Color.white
          >> Clickable (message address <| HoldStitch id)

  in
    Circle 0 0 0.4
    |> LineStyle (Stitch.color stitch) 0.1
    |> fillAndAction
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


