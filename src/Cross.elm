module Cross where

import Graphics.Element exposing (..)
import Graphics.Input exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import Signal exposing (Address, message)
import Effects exposing (Effects)

type Cross = Cross Color | Empty

type Action = Stitch Color | Undo

init : (Model, Effects Action)
init =

update : Action -> Cross -> (Cross, Effects Acction)
update

view : Address Action -> Cross -> Element
view address cross =
  let element = collage 15 15 (crossForm cross)
  in  clickable (message address )


crossForm : Cross -> List Form
crossForm cross =
  case cross of
    Empty -> []
    Cross clr ->
      let style = { defaultLine | color = clr, cap = Round , width = 4 }
      in  List.map (traced style)
            <| List.map2 segment [ (5,5),(-5,5) ] [ (-5,-5),(5,-5) ]

cross = Cross red
mailbox = Signal.mailbox Stitch


main : Element
main = view mailbox.address cross



