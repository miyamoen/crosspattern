module SewingKit.Pattern.Square
  ( Square, Position, Content(..)
  , init
  , x, y, position
  , content, maybeContent
  , Action(..), update
  ) where

(=>) = (,)

(?) = flip Maybe.withDefault


type alias Square content =
  (Position, Content content)


type alias Position =
  (Int, Int)


type Content content
  = Content content
  | NoContent


init : Int -> Int -> Square content
init x y =
  (x => y) => NoContent


-- Action

type Action content
  = Modify content
  | Empty
  | NoOp


update : Action content -> Square content -> Square content
update action sqr =
  case action of
    Modify content ->
      position sqr => Content content

    Empty ->
      position sqr => NoContent

    NoOp ->
      sqr


-- Content

content : Square content -> Content content
content =
  snd


maybeContent : Square a -> Maybe a
maybeContent sqr =
  case content sqr of
    Content c ->
      Just c

    NoContent ->
      Nothing


-- Position

position : Square content -> Position
position =
  fst


x : Square content -> Int
x  =
  position >> fst



y : Square content -> Int
y  =
  position >> snd