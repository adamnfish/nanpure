module Subs exposing (navigate, input, zoom)

import Browser.Events
import Html.Events
import Json.Decode as Json

import Grid exposing (Number (..))
import Model exposing (Model (..), Selection (..))
import Msg exposing (Msg (..))


navigate : Sub Msg
navigate =
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

zoom : Model -> Sub Msg
zoom model =
  case model of
    Playing _ SelectedGrid ->
      Browser.Events.onKeyDown
        ( Json.map ( zoomKeys SelectSquare ) Html.Events.keyCode )
    Playing _ ( SelectedSquare num ) ->
      Browser.Events.onKeyDown
        ( Json.map ( zoomKeys ( SelectSquareCell num ) ) Html.Events.keyCode )
    Playing _ _ ->
      Browser.Events.onKeyDown
        ( Json.map selectGrid Html.Events.keyCode )
    _ ->
      Sub.none

selectGrid : Int -> Msg
selectGrid keyCode =
  if keyCode == 48 then
    SelectGrid
  else
    NoOp

zoomKeys : ( Number -> Msg ) -> Int -> Msg
zoomKeys f keyCode =
  case keyCode of
    49 -> f One
    50 -> f Two
    51 -> f Three
    52 -> f Four
    53 -> f Five
    54 -> f Six
    55 -> f Seven
    56 -> f Eight
    57 -> f Nine

    _ -> NoOp

input : Model -> Sub Msg
input model =
  case model of
    Playing _ ( SelectedCell location ) ->
      Browser.Events.onKeyDown
        ( Json.map ( keyCodeToInputMsg location ) Html.Events.keyCode )
    _ ->
      Sub.none

keyCodeToInputMsg : (Number, Number) -> Int -> Msg
keyCodeToInputMsg location keyCode =
  case keyCode of
    -- DELETE / BACKSPACE
    46  -> Delete location
    8   -> Delete location
    -- ONE
    49  -> Enter location One
    97  -> Enter location One
    -- TWO
    50  -> Enter location Two
    98  -> Enter location Two
    -- THREE
    51  -> Enter location Three
    99  -> Enter location Three
    -- FOUR
    52  -> Enter location Four
    100 -> Enter location Four
    -- FIVE
    53  -> Enter location Five
    101 -> Enter location Five
    -- SIX
    54  -> Enter location Six
    102 -> Enter location Six
    -- SEVEN
    55  -> Enter location Seven
    103 -> Enter location Seven
    -- Eight
    56  -> Enter location Eight
    104 -> Enter location Eight
    -- NINE
    57  -> Enter location Nine
    105 -> Enter location Nine

    _ -> NoOp
