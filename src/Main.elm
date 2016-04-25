import StartApp.Simple exposing (start)

import Html exposing (..)

import SewingKit.Svg exposing (..)

--import SewingKit.Color exposing (..)
--import SewingKit.StitchList exposing (..)
import SewingKit.Pattern exposing (..)


--main = start { model = (init (degrees 90) 1.0 0.5), update = update, view = view }
--main = start { model = init 25 25, update = update, view = view }

main = start { model = init, update = update, view = view }


sid = ChildId 3 (Id 0)
m = init
--main =
--  div []
--  [ text <| toString <| stitchById sid m.patternZipper
--  ]