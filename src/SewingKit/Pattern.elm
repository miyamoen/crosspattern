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


type alias MeshListAction =
  MeshList.Action StitchId


type alias Mesh =
  Mesh.Mesh StitchId


type alias MeshAction =
  Mesh.Action StitchId


type alias Model =
  { zipper : Zipper Pattern
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
  { zipper =
    ( Tree ( Root MeshList.init StitchList.init ) [], [] )
  , holding = HStitch 0
  , hasGrid = True
  }


type Action
  = GoToRoot
  | ModifyStitchList StitchList.Action
  | ModifyMeshList MeshListAction
  | HoldStitch Int
  --| HoldStamp Int Stamp
  | Unhold
  | SwitchGrid
  | NoOp


update : Action -> Model -> Model
update action model =
  case action of
    GoToRoot ->
      { model | zipper = goToRoot model.zipper }

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
        Root mshs stchs ->
          Root mshs (StitchList.update action stchs)

        Node id mshs stchs ->
          Node id mshs (StitchList.update action stchs)

  in
    { model | zipper
      = Zipper.updateDatum updatePattern model.zipper
      ? model.zipper
    }


updateMeshList : MeshListAction -> Model -> Model
updateMeshList action model =
  let
    updatePattern p =
      case p of
        Root mshs stchs ->
          Root (MeshList.update action mshs) stchs

        Node id mshs stchs ->
          Node id (MeshList.update action mshs) stchs

  in
    { model | zipper
      = Zipper.updateDatum updatePattern model.zipper
      ? model.zipper
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


stitchById : Zipper Pattern -> StitchId -> Maybe Stitch
stitchById zipper sid =
  case sid of
    Id id ->
      StitchList.stitch id (stitchList zipper)

    --_ ->
    --  Nothing
    ChildId id sid' ->
      goToChild id zipper
      ?> (flip stitchById) sid'


meshList : Zipper Pattern -> MeshList
meshList zipper =
  case pattern zipper of
    Root m _ ->
      m

    Node _ m _ ->
      m


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
    (meshList <| goToRoot model.zipper)
  , gridButton address model
  , holdPanel address model
  , StitchList.view
    (forwardTo address ModifyStitchList)
    (stitchList <| goToRoot model.zipper)
  ]



-- Mesh View

viewMeshs : Model -> Address MeshListAction -> MeshList -> Html
viewMeshs model address mshs =
  let
    grid elms =
      if model.hasGrid
        then
          Group (MeshList.gridElement mshs :: elms)

        else
          Group elms

    mads : List (Address MeshAction)
    mads =
      MeshList.positions mshs
      |> List.map (forwardTo address << MeshList.Modify)


    msgs : List Message
    msgs =
      List.map2 (meshMessage model.holding) mads mshs

    maybeStitches : List (Maybe Stitch)
    maybeStitches =
      List.map Mesh.maybeContent mshs
      |> List.map (\msid -> msid ?> stitchById model.zipper)

    svg =
      List.map2 meshElement maybeStitches mshs
      |> List.map2 Clickable msgs
      |> grid
      |> MySvg.toSvg []


    attrs =
    [ viewBox
      ((MeshList.minX mshs ? 0) - 2 |> toFloat)
      ((MeshList.minY mshs ? 0) - 2 |> toFloat)
      (MeshList.width mshs + 4 |> toFloat)
      (MeshList.height mshs + 4 |> toFloat)
    , MySvg.width 500
    , MySvg.height 500
    ]

  in
    div
    [ style [] ]
    [ Svg.svg attrs [ svg ] ]


meshElement : Maybe Stitch -> Mesh -> Element
meshElement maybeStitch msh =
  let
    x = Mesh.x msh |> toFloat
    y = Mesh.y msh |> toFloat

    cross sqr =
      maybeStitch
      ?. Stitch.element (x + 0.5) (y + 0.5)
      ?. (\cross -> Group [ sqr, cross ])
      ? Group [ sqr ]

  in
    Square (x + 0.5) (y + 0.5) 1
    |> Opacity 1.0 0
    |> cross


meshMessage : Holding -> Address MeshAction -> Mesh -> Message
meshMessage holding address msh =
  (case holding of
    HStitch id ->
      if Mesh.maybeContent msh ?. (/=) (Id id) ? True then
        Mesh.Modify (Id id)

      else
        Mesh.Empty

    HNothing ->
      Mesh.Empty
  ) |> message address


-- Holding View


holdPanel : Address Action -> Model -> Html
holdPanel address model =
  let
    stitches = stitchList <| goToRoot model.zipper

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


