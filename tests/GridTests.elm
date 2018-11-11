module GridTests exposing (suite, distinct)

import Expect
import Fuzz exposing (Fuzzer)
import List.Extra
import Set exposing (Set)
import Test exposing (..)

import Grid exposing
  (Grid, Number (..), CellValue (..)
  , emptyGrid, getCell, setCell, clearCell, puzzle, cellInSquare
  , cellBySquareAndCellIndices, getCellSquare, getSquare
  )


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

cellResultEquals : (Number, Number) -> CellValue -> Result String Grid -> Expect.Expectation
cellResultEquals (x, y) expected result =
  case result of
    Err msg ->
      Expect.fail ( "grid update failed: " ++ msg )
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
      , describe "cellInSquare"
        [ test "(One, One) should be in square One" <|
          \_ ->
            Expect.equal True ( cellInSquare One (One, One) )
        , test "(One, Two) should be in square One" <|
          \_ ->
            Expect.equal True ( cellInSquare One (One, Two) )
        , test "(One, Three) should be in square One" <|
          \_ ->
            Expect.equal True ( cellInSquare One (One, Three) )
        , test "(One, Four) should be in square Four" <|
          \_ ->
            Expect.equal True ( cellInSquare Four (One, Four) )
        , test "(One, Five) should be in square Four" <|
          \_ ->
            Expect.equal True ( cellInSquare Four (One, Five) )
        , test "(One, Six) should be in square Four" <|
          \_ ->
            Expect.equal True ( cellInSquare Four (One, Six) )
        , test "(One, Seven) should be in square Seven" <|
          \_ ->
            Expect.equal True ( cellInSquare Seven (One, Seven) )
        , test "(One, Eight) should be in square Seven" <|
          \_ ->
            Expect.equal True ( cellInSquare Seven (One, Eight) )
        , test "(One, Nine) should be in square Seven" <|
          \_ ->
            Expect.equal True ( cellInSquare Seven (One, Nine) )
        , test "(Three, Three) should be in square One" <|
          \_ ->
            Expect.equal True ( cellInSquare One (Three, Three) )
        , test "(Four, One) should be in square Two" <|
          \_ ->
            Expect.equal True ( cellInSquare Two (Four, One) )
        , test "(Nine, Nine) should be in square Nine" <|
          \_ ->
            Expect.equal True ( cellInSquare Nine (Nine, Nine) )
        -- negative tests
        , test "(One, One) should not be in square Nine" <|
          \_ ->
            Expect.equal False ( cellInSquare Nine (One, One) )
        , test "(Four, One) should not be in square One" <|
          \_ ->
            Expect.equal False ( cellInSquare One (Four, One) )
        ]
      , describe "cellBySquareAndCellIndices"
        [ test "retrieves the correct cell for One One" <|
          \_ ->
            Expect.equal (One, One) ( cellBySquareAndCellIndices One One )
        , test "retrieves the correct cell for Four Four" <|
          \_ ->
            Expect.equal (One, Five) ( cellBySquareAndCellIndices Four Four )
        , test "retrieves the correct cell for Two Two" <|
          \_ ->
            Expect.equal (Five, One) ( cellBySquareAndCellIndices Two Two )
        ]
      , describe "getCellSquare"
        [ test "returns One for (One, One)" <|
          \_ ->
            Expect.equal One ( getCellSquare (One, One))
        , test "returns One for (Three, Three)" <|
          \_ ->
            Expect.equal One ( getCellSquare (Three, Three))
        , test "returns Three for (Seven, Two)" <|
          \_ ->
            Expect.equal Three ( getCellSquare (Seven, Two))
        , test "returns Five for (Six, Four)" <|
          \_ ->
            Expect.equal Five ( getCellSquare (Six, Four))
        , test "returns Nine for (Eight, Nine)" <|
          \_ ->
            Expect.equal Nine ( getCellSquare (Eight, Nine))
        , test "returns Nine for (Nine, Nine)" <|
          \_ ->
            Expect.equal Nine ( getCellSquare (Nine, Nine))
        ]
      , describe "getSquare"
        [ test "Returns square one in a puzzle" <|
          \_ ->
            let
              gridRes =
                puzzle
                  [ ((One, One), One)
                  , ((Two, One), Two)
                  , ((Three, One), Three)
                  , ((One, Two), Four)
                  , ((Two, Two), Five)
                  , ((Three, Two), Six)
                  , ((One, Three), Seven)
                  , ((Two, Three), Eight)
                  , ((Three, Three), Nine)
                  ]
              expected =
                { c1 = Fixed One
                , c2 = Fixed Two
                , c3 = Fixed Three
                , c4 = Fixed Four
                , c5 = Fixed Five
                , c6 = Fixed Six
                , c7 = Fixed Seven
                , c8 = Fixed Eight
                , c9 = Fixed Nine
                }
            in
              case gridRes of
                Err message ->
                  Expect.fail message
                Ok grid ->
                  Expect.equal
                    ( getSquare One grid )
                    expected
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