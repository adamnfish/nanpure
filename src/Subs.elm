module Subs exposing (navigate)

import Browser.Events
import Html.Events
import Json.Decode as Json

import Model exposing (Model)
import Msg exposing (Msg (..))


navigate : Model -> Sub Msg
navigate model =
  Browser.Events.onKeyDown
    ( Json.map keyCodeToNavMsg Html.Events.keyCode )

keyCodeToNavMsg : Int -> Msg
keyCodeToNavMsg keyCode =
  case keyCode of
    -- ESCAPE
    27 -> Deselect

    -- WASD, HJKL, ARROWS
    38 -> SelectDown
    87 -> SelectDown
    75 -> SelectDown
    
    40 -> SelectUp
    83 -> SelectUp
    74 -> SelectUp
    
    37 -> SelectLeft
    65 -> SelectLeft
    72 -> SelectLeft
    
    39 -> SelectRight
    68 -> SelectRight
    76 -> SelectRight
    
    _ -> NoOp
