module Main exposing (..)

import Browser

import Grid exposing (Number (..), Grid, CellValue (..), Cells, puzzle, numberFromIndex)
import Model exposing (Model (..), Flags, Selection (..))
import Msg exposing (Msg (..), update)
import Subs exposing (navigate, input, zoom)
import Utils exposing (resTraverse)
import View exposing (view)


unpackPuzzle : List ((Int, Int), Int) -> Result String ( List ((Number, Number), Number) )
unpackPuzzle inputs =
  let
    f = \((x, y), n) ->
      Result.map3
        ( \x_ y_ n_ -> ((x_, y_), n_) )
        ( numberFromIndex x )
        ( numberFromIndex y )
        ( numberFromIndex n )
  in
    resTraverse f inputs

init : Flags -> ( Model, Cmd Msg )
init flags =
  let
    gridResult =
      unpackPuzzle flags.fixedCells
      |> Result.andThen puzzle
  in
    case gridResult of
      Err msg ->
        ( Error msg
        , Cmd.none
        )
      Ok grid ->
        ( Playing grid NoSelection
        , Cmd.none
        )

main : Program Flags Model Msg
main =
  Browser.element
    { view = view
    , init = init
    , update = update
    , subscriptions = \model -> Sub.batch [ navigate, input model, zoom model ]
    }
