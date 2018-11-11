module View exposing (view)

import Html exposing (Html, text, div, h1, img, text)
import Html.Attributes exposing (src, class)
import Html.Events
import Svg exposing (svg)
import Svg.Attributes exposing
  (height, width, stroke, strokeWidth, fill, x, y, x1, x2, y1, y2, fontSize)
import Svg.Events

import Grid exposing
  ( Grid, Number (..), CellValue (..)
  , getCell, numberAsString, numberAsIndex
  , getCellSquare, cellInSquare, getSquare, getRow, getCol
  )
import Model exposing (Model (..), Selection (..))
import Msg exposing (Msg (..))
import Puzzle exposing (valid)
import Utils exposing (flattenList)


view : Model -> Html Msg
view model =
  case model of
    Error msg ->
      div [ class "nanpure--container" ]
        [ text msg ]
    Playing grid selection ->
      div [ class "nanpure--container" ]
        [ ( gridDisplay selection grid )
        ]
    Completed grid ->
      div [ class "nanpure--container" ]
        [ ( gridDisplay NoSelection grid )
        ]

gridDisplay : Selection -> Grid -> Html Msg
gridDisplay selection grid =
  let
    cellsEls = cells selection grid
    dividers =
      [ Svg.line
        [ x1 "0"
        , x2 "450"
        , y1 "149"
        , y2 "149"
        , strokeWidth "3"
        ]
        []
      , Svg.line
        [ x1 "0"
        , x2 "450"
        , y1 "299"
        , y2 "299"
        , strokeWidth "3"
        ]
        []
      , Svg.line
        [ x1 "149"
        , x2 "149"
        , y1 "0"
        , y2 "450"
        , strokeWidth "3"
        ]
        []
      , Svg.line
        [ x1 "299"
        , x2 "299"
        , y1 "0"
        , y2 "450"
        , strokeWidth "3"
        ]
        []
      ]
  in
    Svg.svg
      [ height "450"
      , width "450"
      , stroke "black"
      ]
      ( cellsEls ++ dividers )

displayCell : Selection -> (Number, Number) -> Grid -> List ( Html Msg )
displayCell selection (xNum, yNum) grid =
  let
    cellValue = getCell (xNum, yNum) grid
    selected =
      case selection of
        NoSelection ->
          False
        SelectedCell (xSel, ySel) ->
          xNum == xSel && yNum == ySel
        SelectedGrid ->
          True
        SelectedSquare num ->
          cellInSquare num (xNum, yNum)
    squareCells = getSquare ( getCellSquare (xNum, yNum) ) grid
    rowCells = getRow yNum grid
    colCells = getCol xNum grid
    cellIsValid =
      valid squareCells && valid rowCells && valid colCells
  in
    case cellValue of
      Empty ->
        cellHtml "" (xNum, yNum) False selected cellIsValid
      Input num ->
        cellHtml ( numberAsString num ) (xNum, yNum) False selected cellIsValid
      Fixed num ->
        cellHtml ( numberAsString num ) (xNum, yNum) True selected cellIsValid

cellHtml : String -> (Number, Number) -> Bool -> Bool -> Bool -> List ( Html Msg )
cellHtml contents (xNum, yNum) fixed selected isValid =
  let
    xi = numberAsIndex xNum
    yi = numberAsIndex yNum
    xTextOffset = 22
    yTextOffset = 29
    fillColour =
      if fixed then
        if selected then
          "#999999"
        else
          if isValid then
            "#cccccc"
          else
            "#cc9999"
      else
        if selected then
          "#cccc88"
        else
          if isValid then
            "#ffffff"
          else
            "#ffcccc"
  in
    [ Svg.rect
      [ Svg.Attributes.class "nanpure--cell"
      , height "50"
      , width "50"
      , stroke "black"
      , x ( String.fromInt ( 50 * xi ) )
      , y ( String.fromInt ( 50 * yi ) )
      , fill fillColour
      ]
      []
    , Svg.text_
      [ x ( String.fromInt ( 50 * xi + xTextOffset ) )
      , y ( String.fromInt ( 50 * yi + yTextOffset ) )
      ]
      [ text contents ]
    ]

cells : Selection -> Grid -> List ( Html Msg )
cells selection grid =
  flattenList
    [ displayCell selection (One, One) grid
    , displayCell selection (Two, One) grid
    , displayCell selection (Three, One) grid
    , displayCell selection (Four, One) grid
    , displayCell selection (Five, One) grid
    , displayCell selection (Six, One) grid
    , displayCell selection (Seven, One) grid
    , displayCell selection (Eight, One) grid
    , displayCell selection (Nine, One) grid
    , displayCell selection (One, Two) grid
    , displayCell selection (Two, Two) grid
    , displayCell selection (Three, Two) grid
    , displayCell selection (Four, Two) grid
    , displayCell selection (Five, Two) grid
    , displayCell selection (Six, Two) grid
    , displayCell selection (Seven, Two) grid
    , displayCell selection (Eight, Two) grid
    , displayCell selection (Nine, Two) grid
    , displayCell selection (One, Three) grid
    , displayCell selection (Two, Three) grid
    , displayCell selection (Three, Three) grid
    , displayCell selection (Four, Three) grid
    , displayCell selection (Five, Three) grid
    , displayCell selection (Six, Three) grid
    , displayCell selection (Seven, Three) grid
    , displayCell selection (Eight, Three) grid
    , displayCell selection (Nine, Three) grid
    , displayCell selection (One, Four) grid
    , displayCell selection (Two, Four) grid
    , displayCell selection (Three, Four) grid
    , displayCell selection (Four, Four) grid
    , displayCell selection (Five, Four) grid
    , displayCell selection (Six, Four) grid
    , displayCell selection (Seven, Four) grid
    , displayCell selection (Eight, Four) grid
    , displayCell selection (Nine, Four) grid
    , displayCell selection (One, Five) grid
    , displayCell selection (Two, Five) grid
    , displayCell selection (Three, Five) grid
    , displayCell selection (Four, Five) grid
    , displayCell selection (Five, Five) grid
    , displayCell selection (Six, Five) grid
    , displayCell selection (Seven, Five) grid
    , displayCell selection (Eight, Five) grid
    , displayCell selection (Nine, Five) grid
    , displayCell selection (One, Six) grid
    , displayCell selection (Two, Six) grid
    , displayCell selection (Three, Six) grid
    , displayCell selection (Four, Six) grid
    , displayCell selection (Five, Six) grid
    , displayCell selection (Six, Six) grid
    , displayCell selection (Seven, Six) grid
    , displayCell selection (Eight, Six) grid
    , displayCell selection (Nine, Six) grid
    , displayCell selection (One, Seven) grid
    , displayCell selection (Two, Seven) grid
    , displayCell selection (Three, Seven) grid
    , displayCell selection (Four, Seven) grid
    , displayCell selection (Five, Seven) grid
    , displayCell selection (Six, Seven) grid
    , displayCell selection (Seven, Seven) grid
    , displayCell selection (Eight, Seven) grid
    , displayCell selection (Nine, Seven) grid
    , displayCell selection (One, Eight) grid
    , displayCell selection (Two, Eight) grid
    , displayCell selection (Three, Eight) grid
    , displayCell selection (Four, Eight) grid
    , displayCell selection (Five, Eight) grid
    , displayCell selection (Six, Eight) grid
    , displayCell selection (Seven, Eight) grid
    , displayCell selection (Eight, Eight) grid
    , displayCell selection (Nine, Eight) grid
    , displayCell selection (One, Nine) grid
    , displayCell selection (Two, Nine) grid
    , displayCell selection (Three, Nine) grid
    , displayCell selection (Four, Nine) grid
    , displayCell selection (Five, Nine) grid
    , displayCell selection (Six, Nine) grid
    , displayCell selection (Seven, Nine) grid
    , displayCell selection (Eight, Nine) grid
    , displayCell selection (Nine, Nine) grid
    ]
