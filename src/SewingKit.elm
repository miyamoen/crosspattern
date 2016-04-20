module Cross where

import Graphics.Element exposing (..)
import Graphics.Input exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import Signal exposing (Address, message)
import Effects exposing (Effects)
import Array exposing (Array, fromList, push, set, slice)

type alias Thread = Color
type Stitch = Full | UpToRight | UpToLeft

type alias SewingKit =
  { holding : Maybe Thread
  , threads : Array Thread
  , stitch  : Stitch
  }

type Action = Hold (Maybe Thread)
            | Shift Stitch
            | Replace Int Thread
            | Remove  Int
            | Add Thread

update : Action -> SewingKit -> (SewingKit, Effects Acction)
update action sw = case action of
  Hold th        -> (holdThread th sw,        Effects.none)
  Shift st       -> (stitch st sw,            Effects.none)
  Replace idx th -> (replaceThread idx th sw, Effects.none)
  Remove idx     -> (removeThread idx sw,     Effects.none)
  Add th         -> (addThread th sw,         Effects.none)


defaultSewingKit : SewingKit
defaultSewingKit =
  { holding = Nothing
  , threads = fromList [ red, orange, yellow, green, blue, purple ]
  , stitch  = full
  }

holdThread : Maybe Thread -> SewingKit -> SewingKit
holdThread thread sewingKit =
  { sewingKit | holding <- thread }

threads : Array Thread -> SewingKit -> SewingKit
threads threads sewingKit =
  { sewingKit | threads <- threads }

stitch : Stitch -> SewingKit -> SewingKit
stitch stitch sewingKit =
  { sewingKit | stitch <- stitch }

addThread : Thread -> SewingKit -> SewingKit
addThread th sw =
  threads (push th sw.threads) sw

replaceThread : Int -> Thread -> SewingKit -> SewingKit
replaceThread idx th sw =
  threads (set idx th sw.threads) sw

removeThread : Int -> SewingKit -> SewingKit
removeThread idx sw =
  let pre  = slice 0 idx sw.threads
      post = slice (idx + 1) (Array.length sw.threads) sw.threads
  in  threads (Array.append pre post) sw

thread : Color -> Thread
thread clr = clr

stitchElement : Thread -> Stitch -> Element
stitchElement th st =
  let style = { defaultLine | color = th, cap = Round , width = 4 }
      clg   = (collage 15 15) << traced style
  in case st of
      Full      -> layers (List.map (stitchElement th) [ UpToRight,UpToLeft ])
      UpToRight -> clg (segment (-5,-5) (5,5))
      UpToLeft  -> clg (segment (5,-5) (-5,5))

full : Stitch
full = Full

upToRight : Stitch
upToRight = UpToRight

upToLeft : Stitch
upToLeft = UpToLeft

init : (Model, Effects Action)
init =


view : Address Action -> Cross -> Element
view address cross =
  let element = collage 15 15 (crossForm cross)
  in  clickable (message address )



main : Element
main = view mailbox.address cross


colors : Array.Array Color
colors = Array.fromList [red, orange, yellow, green, blue, purple, brown, charcoal, grey, black]

randomColor : Generator Color
randomColor = Random.map (\n -> Maybe.withDefault orange (Array.get n colors)) (Random.int 0 (List.length colors))




