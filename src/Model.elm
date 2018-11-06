module Model exposing (Selection (..), Model)

import Grid exposing (Grid, Number (..))


type Selection
    = NoSelection
    | SeletedCell (Number, Number)
    | SelectedSquare Number

type alias Model =
    { grid : Grid
    , selection : Selection
    }
