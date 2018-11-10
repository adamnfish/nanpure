module View exposing (view)

import Html exposing (Html, text, div, h1, img, text)
import Html.Attributes exposing (src, class)
import Html.Events
import Svg exposing (svg)
import Svg.Attributes exposing
  (height, width, stroke, strokeWidth, fill, x, y, x1, x2, y1, y2, fontSize)
import Svg.Events

import Grid exposing (Grid, Number (..), CellValue (..), getCell, numberAsString, numberAsIndex, cellInSquare)
import Model exposing (Model (..), Selection (..))
import Msg exposing (Msg (..))
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

displayCell : Selection -> CellValue -> (Number, Number) -> List ( Html Msg )
displayCell selection cellValue (xNum, yNum) =
  let
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
  in
    case cellValue of
      Empty ->
        cellHtml "" (xNum, yNum) False selected
      Input num ->
        cellHtml ( numberAsString num ) (xNum, yNum) False selected
      Fixed num ->
        cellHtml ( numberAsString num ) (xNum, yNum) True selected

cellHtml : String -> (Number, Number) -> Bool -> Bool -> List ( Html Msg )
cellHtml contents (xNum, yNum) fixed selected =
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
          "#cccccc"
      else
        if selected then
          "#cccc88"
        else
          "#ffffff"
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
    [ displayCell selection ( getCell (One, One) grid ) (One, One)
    , displayCell selection ( getCell (Two, One) grid ) (Two, One)
    , displayCell selection ( getCell (Three, One) grid ) (Three, One)
    , displayCell selection ( getCell (Four, One) grid ) (Four, One)
    , displayCell selection ( getCell (Five, One) grid ) (Five, One)
    , displayCell selection ( getCell (Six, One) grid ) (Six, One)
    , displayCell selection ( getCell (Seven, One) grid ) (Seven, One)
    , displayCell selection ( getCell (Eight, One) grid ) (Eight, One)
    , displayCell selection ( getCell (Nine, One) grid ) (Nine, One)
    , displayCell selection ( getCell (One, Two) grid ) (One, Two)
    , displayCell selection ( getCell (Two, Two) grid ) (Two, Two)
    , displayCell selection ( getCell (Three, Two) grid ) (Three, Two)
    , displayCell selection ( getCell (Four, Two) grid ) (Four, Two)
    , displayCell selection ( getCell (Five, Two) grid ) (Five, Two)
    , displayCell selection ( getCell (Six, Two) grid ) (Six, Two)
    , displayCell selection ( getCell (Seven, Two) grid ) (Seven, Two)
    , displayCell selection ( getCell (Eight, Two) grid ) (Eight, Two)
    , displayCell selection ( getCell (Nine, Two) grid ) (Nine, Two)
    , displayCell selection ( getCell (One, Three) grid ) (One, Three)
    , displayCell selection ( getCell (Two, Three) grid ) (Two, Three)
    , displayCell selection ( getCell (Three, Three) grid ) (Three, Three)
    , displayCell selection ( getCell (Four, Three) grid ) (Four, Three)
    , displayCell selection ( getCell (Five, Three) grid ) (Five, Three)
    , displayCell selection ( getCell (Six, Three) grid ) (Six, Three)
    , displayCell selection ( getCell (Seven, Three) grid ) (Seven, Three)
    , displayCell selection ( getCell (Eight, Three) grid ) (Eight, Three)
    , displayCell selection ( getCell (Nine, Three) grid ) (Nine, Three)
    , displayCell selection ( getCell (One, Four) grid ) (One, Four)
    , displayCell selection ( getCell (Two, Four) grid ) (Two, Four)
    , displayCell selection ( getCell (Three, Four) grid ) (Three, Four)
    , displayCell selection ( getCell (Four, Four) grid ) (Four, Four)
    , displayCell selection ( getCell (Five, Four) grid ) (Five, Four)
    , displayCell selection ( getCell (Six, Four) grid ) (Six, Four)
    , displayCell selection ( getCell (Seven, Four) grid ) (Seven, Four)
    , displayCell selection ( getCell (Eight, Four) grid ) (Eight, Four)
    , displayCell selection ( getCell (Nine, Four) grid ) (Nine, Four)
    , displayCell selection ( getCell (One, Five) grid ) (One, Five)
    , displayCell selection ( getCell (Two, Five) grid ) (Two, Five)
    , displayCell selection ( getCell (Three, Five) grid ) (Three, Five)
    , displayCell selection ( getCell (Four, Five) grid ) (Four, Five)
    , displayCell selection ( getCell (Five, Five) grid ) (Five, Five)
    , displayCell selection ( getCell (Six, Five) grid ) (Six, Five)
    , displayCell selection ( getCell (Seven, Five) grid ) (Seven, Five)
    , displayCell selection ( getCell (Eight, Five) grid ) (Eight, Five)
    , displayCell selection ( getCell (Nine, Five) grid ) (Nine, Five)
    , displayCell selection ( getCell (One, Six) grid ) (One, Six)
    , displayCell selection ( getCell (Two, Six) grid ) (Two, Six)
    , displayCell selection ( getCell (Three, Six) grid ) (Three, Six)
    , displayCell selection ( getCell (Four, Six) grid ) (Four, Six)
    , displayCell selection ( getCell (Five, Six) grid ) (Five, Six)
    , displayCell selection ( getCell (Six, Six) grid ) (Six, Six)
    , displayCell selection ( getCell (Seven, Six) grid ) (Seven, Six)
    , displayCell selection ( getCell (Eight, Six) grid ) (Eight, Six)
    , displayCell selection ( getCell (Nine, Six) grid ) (Nine, Six)
    , displayCell selection ( getCell (One, Seven) grid ) (One, Seven)
    , displayCell selection ( getCell (Two, Seven) grid ) (Two, Seven)
    , displayCell selection ( getCell (Three, Seven) grid ) (Three, Seven)
    , displayCell selection ( getCell (Four, Seven) grid ) (Four, Seven)
    , displayCell selection ( getCell (Five, Seven) grid ) (Five, Seven)
    , displayCell selection ( getCell (Six, Seven) grid ) (Six, Seven)
    , displayCell selection ( getCell (Seven, Seven) grid ) (Seven, Seven)
    , displayCell selection ( getCell (Eight, Seven) grid ) (Eight, Seven)
    , displayCell selection ( getCell (Nine, Seven) grid ) (Nine, Seven)
    , displayCell selection ( getCell (One, Eight) grid ) (One, Eight)
    , displayCell selection ( getCell (Two, Eight) grid ) (Two, Eight)
    , displayCell selection ( getCell (Three, Eight) grid ) (Three, Eight)
    , displayCell selection ( getCell (Four, Eight) grid ) (Four, Eight)
    , displayCell selection ( getCell (Five, Eight) grid ) (Five, Eight)
    , displayCell selection ( getCell (Six, Eight) grid ) (Six, Eight)
    , displayCell selection ( getCell (Seven, Eight) grid ) (Seven, Eight)
    , displayCell selection ( getCell (Eight, Eight) grid ) (Eight, Eight)
    , displayCell selection ( getCell (Nine, Eight) grid ) (Nine, Eight)
    , displayCell selection ( getCell (One, Nine) grid ) (One, Nine)
    , displayCell selection ( getCell (Two, Nine) grid ) (Two, Nine)
    , displayCell selection ( getCell (Three, Nine) grid ) (Three, Nine)
    , displayCell selection ( getCell (Four, Nine) grid ) (Four, Nine)
    , displayCell selection ( getCell (Five, Nine) grid ) (Five, Nine)
    , displayCell selection ( getCell (Six, Nine) grid ) (Six, Nine)
    , displayCell selection ( getCell (Seven, Nine) grid ) (Seven, Nine)
    , displayCell selection ( getCell (Eight, Nine) grid ) (Eight, Nine)
    , displayCell selection ( getCell (Nine, Nine) grid ) (Nine, Nine)
    ]
