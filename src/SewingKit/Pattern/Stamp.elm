module SewingKit.Pattern.Stamp
  ( Stamp(..)
  ) where

import Html exposing (..)
import Html.Attributes exposing (style)
import Svg
import Color exposing (Color)
import Signal exposing (Address, message, Message, forwardTo)
import List.Extra exposing (lift2)
import Array exposing (indexedMap, toList)
import Maybe exposing (andThen)

import SewingKit.Svg exposing (..)
import SewingKit.Stitch as Stitch
import SewingKit.StitchList as Stitches
import SewingKit.Cloth as Cloth


(=>) = (,)

(?) = flip Maybe.withDefault


type Stamp
  = Normal


