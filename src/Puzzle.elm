module Puzzle exposing (valid, complete)

import Grid exposing (CellValue(..), Cells, Grid, Number(..), getCol, getRow, getSquare, isFilled)


valid : Cells -> Bool
valid cells =
  let
    values =
      [ cells.c1
      , cells.c2
      , cells.c3
      , cells.c4
      , cells.c5
      , cells.c6
      , cells.c7
      , cells.c8
      , cells.c9
      ]
    (result, _) = List.foldl
      ( \cellValue (validSoFar, seen) ->
        case cellValue of
          Empty ->
            (validSoFar, seen)
          Fixed num ->
            ( validSoFar && ( not ( List.member num seen ) )
            , num :: seen
            )
          Input num ->
            ( validSoFar && ( not ( List.member num seen ) )
            , num :: seen
            )
      )
      (True, [])
      values
  in
    result

complete : Grid -> Bool
complete grid =
  let
    validRows =
      ( valid <| getRow One grid ) && ( valid <| getRow Two grid ) &&
        ( valid <| getRow Three grid ) && ( valid <| getRow Four grid ) &&
        ( valid <| getRow Five grid ) && ( valid <| getRow Six grid ) &&
        ( valid <| getRow Seven grid ) && ( valid <| getRow Eight grid ) &&
        ( valid <| getRow Nine grid )
    validCols =
      ( valid <| getCol One grid ) && ( valid <| getCol Two grid ) &&
        ( valid <| getCol Three grid ) && ( valid <| getCol Four grid ) &&
        ( valid <| getCol Five grid ) && ( valid <| getCol Six grid ) &&
        ( valid <| getCol Seven grid ) && ( valid <| getCol Eight grid ) &&
        ( valid <| getCol Nine grid )
    validCells =
      ( valid <| getSquare One grid ) && ( valid <| getSquare Two grid ) &&
        ( valid <| getSquare Three grid ) && ( valid <| getSquare Four grid ) &&
        ( valid <| getSquare Five grid ) && ( valid <| getSquare Six grid ) &&
        ( valid <| getSquare Seven grid ) && ( valid <| getSquare Eight grid ) &&
        ( valid <| getSquare Nine grid )
    isFull = isFilled grid
  in
    isFull && validRows && validCols && validCells
