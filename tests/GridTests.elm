module GridTests exposing (suite, distinct)

import Expect
import Fuzz exposing (Fuzzer)
import List.Extra
import Set exposing (Set)
import Test exposing (..)

import Grid exposing (Grid, Number (..), CellValue (..), emptyGrid, getCell, setCell, clearCell, puzzle)


numFuzzer : Fuzzer Number
numFuzzer =
  Fuzz.oneOf
    [ Fuzz.constant One
    , Fuzz.constant Two
    , Fuzz.constant Three
    , Fuzz.constant Four
    , Fuzz.constant Five
    , Fuzz.constant Six
    , Fuzz.constant Seven
    , Fuzz.constant Eight
    , Fuzz.constant Nine
    ]

cellFuzzer : Fuzzer (Number, Number)
cellFuzzer =
  Fuzz.tuple (numFuzzer, numFuzzer)

distinct : List a -> List a
distinct aa =
  List.reverse <| Tuple.first <| List.foldl
    ( \a (acc, seen) ->
      let
        newSeen = a :: seen
      in
      if List.member a seen then
        (acc, newSeen)
      else
        (a :: acc, newSeen)
    )
    ([], [])
    aa

cellsFuzzer : Fuzzer ( List (Number, Number) )
cellsFuzzer =
  Fuzz.map distinct ( Fuzz.list cellFuzzer )

cellResultEquals : (Number, Number) -> CellValue -> Result () Grid -> Expect.Expectation
cellResultEquals (x, y) expected result =
  case result of
    Err () ->
      Expect.fail "grid update failed"
    Ok grid ->
      Expect.equal ( getCell (x, y) grid ) expected


suite : Test
suite =
  describe "grid functions"
    [ describe "for dealing creating grids and puzzles"
      [ describe "empty grid"
        [ fuzz cellFuzzer "sets all cells to Empty" <|
          \(x, y) ->
            Expect.equal ( getCell (x, y) emptyGrid ) Empty
        ]
      , describe "puzzle"
        [ fuzz cellFuzzer "fixes the provided cell" <|
          \(x, y) ->
            let
              gridResult = puzzle [ ((x, y), One) ]
            in
              cellResultEquals (x, y) ( Fixed One ) gridResult
        , fuzz cellsFuzzer "fixes multiple cells" <|
          \cells ->
            let
              cellsWithValues = List.map ( \cell -> (cell, One) ) cells
              gridResult = puzzle cellsWithValues
            in
              case gridResult of
                Err _ ->
                  Expect.fail "Couldn't create puzzle"
                Ok grid ->
                  Expect.true "Did not fix initial values when creating puzzle"
                    ( List.all
                      ( (==) ( Fixed One ) )
                      ( List.map
                        ( \cell -> getCell cell grid )
                        cells
                      )
                    )
        ]
      ]

    , describe "grid manipulations"
      [ describe "setCell"
        [ fuzz cellFuzzer "can set an empty cell to a value" <|
          \(x, y) ->
            Ok emptyGrid
              |> Result.andThen ( setCell (x, y) One )
              |> cellResultEquals (x, y) ( Input One )
        , fuzz cellFuzzer "overwrites an existing value" <|
          \(x, y) ->
            Ok emptyGrid
              |> Result.andThen ( setCell (x, y) One )
              |> Result.andThen ( setCell (x, y) Two )
              |> cellResultEquals (x, y) ( Input Two )
        , fuzz cellFuzzer "fails to set a 'fixed' value" <|
          \(x, y) ->
            puzzle [ ((x, y), One) ]
              |> Result.andThen ( setCell (x, y) Two )
              |> Expect.err
        ]

      , describe "clearCell"
        [ fuzz cellFuzzer "can clear a previously set value" <|
          \(x, y) ->
            Ok emptyGrid
              |> Result.andThen ( setCell (x, y) One )
              |> Result.andThen ( clearCell (x, y) )
              |> cellResultEquals (x, y) Empty
        , fuzz cellFuzzer "clearing an empty cell leaves it empty" <|
          \(x, y) ->
            Ok emptyGrid
              |> Result.andThen ( clearCell (x, y) )
              |> cellResultEquals (x, y) Empty
        , fuzz cellFuzzer "fails to clear a fixed value" <|
          \(x, y) ->
            puzzle [ ((x, y), One) ]
              |> Result.andThen ( clearCell (x, y) )
              |> Expect.err
        ]
      ]
    ]