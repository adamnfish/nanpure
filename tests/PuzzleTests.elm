module PuzzleTests exposing (suite)

import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (..)

import Grid exposing (Number (..), CellValue (..), Cells)
import Puzzle exposing (valid)


makeCells : CellValue -> CellValue -> CellValue -> CellValue -> CellValue -> CellValue -> CellValue -> CellValue -> CellValue -> Cells
makeCells c1 c2 c3 c4 c5 c6 c7 c8 c9 =
  { c1 = c1
  , c2 = c2
  , c3 = c3
  , c4 = c4
  , c5 = c5
  , c6 = c6
  , c7 = c7
  , c8 = c8
  , c9 = c9
  }

suite : Test
suite =
  describe "puzzle functions"
    [ describe "valid"
      [ test "returns true for an empty row" <|
        \_ ->
          let
              cells = ( makeCells Empty Empty Empty Empty Empty Empty Empty Empty Empty )
          in
            Expect.true "was not valid" ( valid cells )
      , test "returns true for a partial row of valid input" <|
        \_ ->
          let
              cells = ( makeCells ( Input One ) Empty Empty ( Input Two ) Empty Empty Empty Empty Empty )
          in
            Expect.true "was not valid" ( valid cells )
      , test "returns true for a complete row of valid input" <|
        \_ ->
          let
              cells =
                ( makeCells
                  ( Input One ) ( Input Two ) ( Input Three )
                  ( Input Four ) ( Input Five ) ( Input Six )
                  ( Input Seven ) ( Input Eight ) ( Input Nine )
                )
          in
            Expect.true "was not valid" ( valid cells )
      , test "returns true for a valid row with input and fixed values" <|
        \_ ->
          let
              cells =
                ( makeCells
                  ( Fixed One ) ( Input Two ) ( Input Three )
                  ( Fixed Four ) ( Input Five ) ( Input Six )
                  ( Fixed Seven ) ( Input Eight ) ( Input Nine )
                )
          in
            Expect.true "was not valid" ( valid cells )
      , test "returns false for an invalid row" <|
        \_ ->
          let
              cells =
                ( makeCells
                  ( Fixed One ) ( Input Two ) ( Input Three )
                  ( Fixed One ) ( Input Five ) ( Input Six )
                  ( Fixed Seven ) ( Input Eight ) ( Input Nine )
                )
          in
            Expect.false "expected invalid, retured true" ( valid cells )
      , test "returns false for a row with invalid incomplete input" <|
        \_ ->
          let
              cells = ( makeCells ( Input One ) ( Input One ) Empty Empty Empty Empty Empty Empty Empty )
          in
            Expect.false "expected invalid, retured true" ( valid cells )
      , test "returns false for a row with input that clashes with fixed values" <|
        \_ ->
          let
              cells = ( makeCells ( Input One ) ( Fixed One ) Empty Empty Empty Empty Empty Empty Empty )
          in
            Expect.false "expected invalid, retured true" ( valid cells )
      ]
    ]
