module Puzzle exposing (valid)

import Grid exposing (Cells, CellValue (..))


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
