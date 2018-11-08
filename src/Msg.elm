module Msg exposing (Msg (..), update)

import Grid exposing (Number (..), numberMod, setCell, clearCell)
import Model exposing (Model (..), Selection (..))


type Msg
  = NoOp
  | Deselect
  | SelectUp
  | SelectDown
  | SelectLeft
  | SelectRight
  -- | SelectGrid
  -- | SelectSquare Number
  -- | SelectSquareCell Number
  | Enter (Number, Number) Number
  | Delete (Number, Number)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case model of
    Error error ->
      ( model, Cmd.none )
    Completed grid ->
      ( model, Cmd.none )
    Playing grid selection ->
      case msg of
        NoOp ->
          ( model, Cmd.none )
        Deselect ->
          ( Playing grid NoSelection
          , Cmd.none
          )
        SelectUp ->
           ( Playing grid ( moveSelection (0, 1) selection )
          , Cmd.none
          )
        SelectDown ->
          ( Playing grid ( moveSelection (0, -1) selection )
          , Cmd.none
          )
        SelectLeft ->
          ( Playing grid ( moveSelection (-1, 0) selection )
          , Cmd.none
          )
        SelectRight ->
          ( Playing grid ( moveSelection (1, 0) selection )
          , Cmd.none
          )
        Enter location value ->
          case ( setCell location value grid ) of
            Err message ->
              ( Error message, Cmd.none )
            Ok updatedGrid ->
              ( Playing updatedGrid selection
              , Cmd.none
              )
        Delete location ->
          case ( clearCell location grid ) of
            Err message ->
              ( Error message, Cmd.none )
            Ok updatedGrid ->
              ( Playing updatedGrid selection
              , Cmd.none
              )

moveSelection : (Int, Int) -> Selection -> Selection
moveSelection (dx, dy) selection =
  case selection of
    NoSelection ->
      SelectedCell (One, One)
    SelectedCell (xNum, yNum) ->
      SelectedCell
        ( numberMod xNum dx
        , numberMod yNum dy
        )
