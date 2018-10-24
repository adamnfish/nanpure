module Grid exposing (..)

import Test exposing (..)
import Fuzz exposing (..)
import Expect

import Models exposing (Number (..), CellValue (..), emptyGrid, getCell, updateCell)

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

suite : Test
suite =
  describe "functions for dealing with grids"
    [ describe "empty grid"
      [ fuzz2 numFuzzer numFuzzer "sets all cells to Empty" <|
        \x y ->
          let
            grid = emptyGrid
          in
            Expect.equal ( getCell (x, y) grid ) Empty
      ]
    , describe "updateCell"
      [ fuzz2 numFuzzer numFuzzer "can set an empty cell to a value" <|
        \x y ->
          let
            grid = emptyGrid
            newGridResult = updateCell (x, y) ( Just One ) grid
          in
            case newGridResult of
              Err () ->
                Expect.fail "grid update failed"
              Ok newGrid ->
                Expect.equal ( getCell (x, y) newGrid ) ( Input One )
      ]
    ]