import StartApp.Simple exposing (start)

--import SewingKit.PopUpingColorPicker
import SewingKit.Svg exposing (..)

--import SewingKit.NeedleAndThreadList
--import SewingKit.Color exposing (..)
--import SewingKit.StitchList exposing (..)
import SewingKit.Pattern exposing (..)


--main = start { model = (init (degrees 90) 1.0 0.5), update = update, view = view }
--main = start { model = init 25 25, update = update, view = view }

main = start { model = init, update = update, view = view }

