module Main exposing (..)

import Browser

import Grid exposing (Number (..), Grid, CellValue (..), Cells, emptyGrid)
import Model exposing (Model, Selection (..))
import View exposing (view)
import Msg exposing (Msg (..), update)


init : ( Model, Cmd Msg )
init =
    ( { grid = emptyGrid
      , selection = NoSelection
      }
    , Cmd.none
    )

main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
