module Events exposing (navigationEvents)

import Html
import Html.Events
import Json.Decode as Json
import Svg
import Svg.Events

import Msg exposing (Msg (..))


onKeyUp : (Int -> Msg) -> Svg.Attribute Msg
onKeyUp tagger =
  Svg.Events.on "keyup" (Json.map tagger Html.Events.keyCode)

navigationEvents : Svg.Attribute Msg
navigationEvents =
  onKeyUp
    ( \keyCode ->
      case keyCode of
        38 -> SelectUp
        87 -> SelectUp
        75 -> SelectUp
        
        40 -> SelectDown
        83 -> SelectDown
        74 -> SelectDown
        
        37 -> SelectLeft
        65 -> SelectLeft
        72 -> SelectLeft
        
        39 -> SelectRight
        68 -> SelectRight
        76 -> SelectRight
        
        _ -> NoOp
    )
