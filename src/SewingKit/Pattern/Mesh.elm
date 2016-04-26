module SewingKit.Pattern.Mesh
  ( Mesh, Position, Content(..)
  , init
  , x, y, position
  , content, maybeContent
  , Action(..), update
  ) where

(=>) = (,)

(?) = flip Maybe.withDefault


type alias Mesh content =
  (Position, Content content)


type alias Position =
  (Int, Int)


type Content content
  = Content content
  | NoContent


init : Int -> Int -> Mesh content
init x y =
  (x => y) => NoContent


-- Action

type Action content
  = Modify content
  | Empty
  | NoOp


update : Action content -> Mesh content -> Mesh content
update action sqr =
  case action of
    Modify content ->
      position sqr => Content content

    Empty ->
      position sqr => NoContent

    NoOp ->
      sqr


-- Content

content : Mesh content -> Content content
content =
  snd


maybeContent : Mesh a -> Maybe a
maybeContent sqr =
  case content sqr of
    Content c ->
      Just c

    NoContent ->
      Nothing


-- Position

position : Mesh content -> Position
position =
  fst


x : Mesh content -> Int
x  =
  position >> fst



y : Mesh content -> Int
y  =
  position >> snd