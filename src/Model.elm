module Model exposing (Selection (..), Model (..), Flags)

import Grid exposing (Grid, Number (..))


type Selection
  = NoSelection
  | SeletedCell (Number, Number)
  | SelectedSquare Number

type Model
  = Error String
  | Playing Grid Selection
  | Completed Grid

type alias Flags =
  { fixedCells : List ((Int, Int), Int)
  }
