module Model exposing (Flags, Model(..), Selection(..))

import Grid exposing (Grid, Number(..))


type Selection
    = NoSelection
    | SelectedCell ( Number, Number )
    | SelectedGrid
    | SelectedSquare Number


type Model
    = Error String
    | Playing Grid Selection
    | Completed Grid


type alias Flags =
    { fixedCells : List ( ( Int, Int ), Int )
    }
