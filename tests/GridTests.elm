module GridTests exposing (..)

import Test exposing (..)
import Fuzz exposing (..)
import Expect

import Grid exposing (Grid, Number (..), CellValue (..), emptyGrid, getCell, setCell, clearCell, puzzle)

-- http://package.elm-lang.org/packages/elm-community/elm-test/latest


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

cellResultEquals : (Number, Number) -> CellValue -> Result () Grid -> Expect.Expectation
cellResultEquals (x, y) expected result =
  case result of
    Err () ->
      Expect.fail "grid update failed"
    Ok grid ->
      Expect.equal ( getCell (x, y) grid ) expected

suite : Test
suite =
  describe "functions for dealing with grids"
    [ describe "empty grid"
      [ fuzz2 numFuzzer numFuzzer "sets all cells to Empty" <|
        \x y ->
          Expect.equal ( getCell (x, y) emptyGrid ) Empty
      ]

    , describe "setCell"
      [ fuzz2 numFuzzer numFuzzer "can set an empty cell to a value" <|
        \x y ->
          Ok emptyGrid
            |> Result.andThen ( setCell (x, y) One )
            |> cellResultEquals (x, y) ( Input One )
      , fuzz2 numFuzzer numFuzzer "overwrites an existing value" <|
        \x y ->
          Ok emptyGrid
            |> Result.andThen ( setCell (x, y) One )
            |> Result.andThen ( setCell (x, y) Two )
            |> cellResultEquals (x, y) ( Input Two )
      , fuzz2 numFuzzer numFuzzer "fails to set a 'fixed' value" <|
        \x y ->
          puzzle [ ((x, y), One) ]
            |> Result.andThen ( setCell (x, y) Two )
            |> Expect.err
      ]

    , describe "clearCell"
      [ fuzz2 numFuzzer numFuzzer "can clear a previously set value" <|
        \x y ->
          Ok emptyGrid
            |> Result.andThen ( setCell (x, y) One )
            |> Result.andThen ( clearCell (x, y) )
            |> cellResultEquals (x, y) Empty
      , fuzz2 numFuzzer numFuzzer "clearing an empty cell leaves it empty" <|
        \x y ->
          Ok emptyGrid
            |> Result.andThen ( clearCell (x, y) )
            |> cellResultEquals (x, y) Empty
      , fuzz2 numFuzzer numFuzzer "fails to clear a fixed value" <|
        \x y ->
          puzzle [ ((x, y), One) ]
            |> Result.andThen ( clearCell (x, y) )
            |> Expect.err
      ]
    ]
