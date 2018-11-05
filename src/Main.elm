module Main exposing (..)

import Browser
import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src)

import Grid exposing (Number (..), Grid, CellValue (..), Cells, emptyGrid)


---- MODEL ----

type Selection
    = NoSelection
    | SeletedCell (Number, Number)
    | SelectedSquare Number

type alias Model =
    { grid : Grid
    , selection : Selection
    }


init : ( Model, Cmd Msg )
init =
    ( { grid = emptyGrid
      , selection = NoSelection
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
