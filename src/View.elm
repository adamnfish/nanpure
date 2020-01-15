module View exposing (view)

import Html exposing (Html, text, div, h1, img, text, button)
import Html.Attributes exposing (src, class, disabled)
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
        [ ( gridDisplay False selection grid )
        , controls selection
        ]
    Completed grid ->
      div [ class "nanpure--container" ]
        [ ( gridDisplay True NoSelection grid )
        ]

controls : Selection -> Html Msg
controls selection =
    case selection of
        SelectedCell (xNum, yNum) ->
            div
              [ class "nanpure--controls" ]
              [ button [ Html.Events.onClick ( Delete (xNum, yNum) )] [ text "x" ]
              , button [ Html.Events.onClick ( Enter (xNum, yNum) One )] [ text "1" ]
              , button [ Html.Events.onClick ( Enter (xNum, yNum) Two )] [ text "2" ]
              , button [ Html.Events.onClick ( Enter (xNum, yNum) Three )] [ text "3" ]
              , button [ Html.Events.onClick ( Enter (xNum, yNum) Four )] [ text "4" ]
              , button [ Html.Events.onClick ( Enter (xNum, yNum) Five )] [ text "5" ]
              , button [ Html.Events.onClick ( Enter (xNum, yNum) Six )] [ text "6" ]
              , button [ Html.Events.onClick ( Enter (xNum, yNum) Seven )] [ text "7" ]
              , button [ Html.Events.onClick ( Enter (xNum, yNum) Eight )] [ text "8" ]
              , button [ Html.Events.onClick ( Enter (xNum, yNum) Nine )] [ text "9" ]
              ]
        _ ->
            div
              [ class "nanpure--controls" ]
              [ button [ disabled True] [ text "x" ]
              , button [ disabled True] [ text "1" ]
              , button [ disabled True] [ text "2" ]
              , button [ disabled True] [ text "3" ]
              , button [ disabled True] [ text "4" ]
              , button [ disabled True] [ text "5" ]
              , button [ disabled True] [ text "6" ]
              , button [ disabled True] [ text "7" ]
              , button [ disabled True] [ text "8" ]
              , button [ disabled True] [ text "9" ]
              ]

gridDisplay : Bool -> Selection -> Grid -> Html Msg
gridDisplay isComplete selection grid =
  let
    cellsEls = cells isComplete selection grid
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

displayCell : Bool -> Selection -> (Number, Number) -> Grid -> List ( Html Msg )
displayCell isComplete selection (xNum, yNum) grid =
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
        cellHtml "" (xNum, yNum) isComplete False selected cellIsValid
      Input num ->
        cellHtml ( numberAsString num ) (xNum, yNum) isComplete False selected cellIsValid
      Fixed num ->
        cellHtml ( numberAsString num ) (xNum, yNum) isComplete True selected cellIsValid

cellHtml : String -> (Number, Number) -> Bool -> Bool -> Bool -> Bool -> List ( Html Msg )
cellHtml contents (xNum, yNum) isComplete fixed selected isValid =
  let
    xi = numberAsIndex xNum
    yi = numberAsIndex yNum
    xTextOffset = 21
    yTextOffset = 30
    fillColour =
      if isComplete then
        "#44ff44"
      else if fixed then
        if selected then
          "#999999"
        else
          if isValid then
            "#cccccc"
          else
            "#cc9999"
      else
        if selected then
          "#ffff44"
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
      , Html.Events.onClick ( SelectCell (xNum, yNum) )
      ]
      []
    , Svg.text_
      [ x ( String.fromInt ( 50 * xi + xTextOffset ) )
      , y ( String.fromInt ( 50 * yi + yTextOffset ) )
      ]
      [ text contents ]
    ]

cells : Bool -> Selection -> Grid -> List ( Html Msg )
cells isComplete selection grid =
  flattenList
    [ displayCell isComplete selection (One, One) grid
    , displayCell isComplete selection (Two, One) grid
    , displayCell isComplete selection (Three, One) grid
    , displayCell isComplete selection (Four, One) grid
    , displayCell isComplete selection (Five, One) grid
    , displayCell isComplete selection (Six, One) grid
    , displayCell isComplete selection (Seven, One) grid
    , displayCell isComplete selection (Eight, One) grid
    , displayCell isComplete selection (Nine, One) grid
    , displayCell isComplete selection (One, Two) grid
    , displayCell isComplete selection (Two, Two) grid
    , displayCell isComplete selection (Three, Two) grid
    , displayCell isComplete selection (Four, Two) grid
    , displayCell isComplete selection (Five, Two) grid
    , displayCell isComplete selection (Six, Two) grid
    , displayCell isComplete selection (Seven, Two) grid
    , displayCell isComplete selection (Eight, Two) grid
    , displayCell isComplete selection (Nine, Two) grid
    , displayCell isComplete selection (One, Three) grid
    , displayCell isComplete selection (Two, Three) grid
    , displayCell isComplete selection (Three, Three) grid
    , displayCell isComplete selection (Four, Three) grid
    , displayCell isComplete selection (Five, Three) grid
    , displayCell isComplete selection (Six, Three) grid
    , displayCell isComplete selection (Seven, Three) grid
    , displayCell isComplete selection (Eight, Three) grid
    , displayCell isComplete selection (Nine, Three) grid
    , displayCell isComplete selection (One, Four) grid
    , displayCell isComplete selection (Two, Four) grid
    , displayCell isComplete selection (Three, Four) grid
    , displayCell isComplete selection (Four, Four) grid
    , displayCell isComplete selection (Five, Four) grid
    , displayCell isComplete selection (Six, Four) grid
    , displayCell isComplete selection (Seven, Four) grid
    , displayCell isComplete selection (Eight, Four) grid
    , displayCell isComplete selection (Nine, Four) grid
    , displayCell isComplete selection (One, Five) grid
    , displayCell isComplete selection (Two, Five) grid
    , displayCell isComplete selection (Three, Five) grid
    , displayCell isComplete selection (Four, Five) grid
    , displayCell isComplete selection (Five, Five) grid
    , displayCell isComplete selection (Six, Five) grid
    , displayCell isComplete selection (Seven, Five) grid
    , displayCell isComplete selection (Eight, Five) grid
    , displayCell isComplete selection (Nine, Five) grid
    , displayCell isComplete selection (One, Six) grid
    , displayCell isComplete selection (Two, Six) grid
    , displayCell isComplete selection (Three, Six) grid
    , displayCell isComplete selection (Four, Six) grid
    , displayCell isComplete selection (Five, Six) grid
    , displayCell isComplete selection (Six, Six) grid
    , displayCell isComplete selection (Seven, Six) grid
    , displayCell isComplete selection (Eight, Six) grid
    , displayCell isComplete selection (Nine, Six) grid
    , displayCell isComplete selection (One, Seven) grid
    , displayCell isComplete selection (Two, Seven) grid
    , displayCell isComplete selection (Three, Seven) grid
    , displayCell isComplete selection (Four, Seven) grid
    , displayCell isComplete selection (Five, Seven) grid
    , displayCell isComplete selection (Six, Seven) grid
    , displayCell isComplete selection (Seven, Seven) grid
    , displayCell isComplete selection (Eight, Seven) grid
    , displayCell isComplete selection (Nine, Seven) grid
    , displayCell isComplete selection (One, Eight) grid
    , displayCell isComplete selection (Two, Eight) grid
    , displayCell isComplete selection (Three, Eight) grid
    , displayCell isComplete selection (Four, Eight) grid
    , displayCell isComplete selection (Five, Eight) grid
    , displayCell isComplete selection (Six, Eight) grid
    , displayCell isComplete selection (Seven, Eight) grid
    , displayCell isComplete selection (Eight, Eight) grid
    , displayCell isComplete selection (Nine, Eight) grid
    , displayCell isComplete selection (One, Nine) grid
    , displayCell isComplete selection (Two, Nine) grid
    , displayCell isComplete selection (Three, Nine) grid
    , displayCell isComplete selection (Four, Nine) grid
    , displayCell isComplete selection (Five, Nine) grid
    , displayCell isComplete selection (Six, Nine) grid
    , displayCell isComplete selection (Seven, Nine) grid
    , displayCell isComplete selection (Eight, Nine) grid
    , displayCell isComplete selection (Nine, Nine) grid
    ]
