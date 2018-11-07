module View exposing (view)

import Html exposing (Html, text, div, h1, img, text)
import Html.Attributes exposing (src, class)
import Html.Events
import Svg exposing (svg)
import Svg.Attributes exposing
  (height, width, stroke, strokeWidth, fill, x, y, x1, x2, y1, y2, fontSize)
import Svg.Events

import Events exposing (navigationEvents)
import Grid exposing (Grid, Number (..), CellValue (..), getCell, numberAsString, numberAsIndex)
import Model exposing (Model (..))
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
        [ ( gridDisplay grid )
        ]
    Completed grid ->
      div [ class "nanpure--container" ]
        [ ( gridDisplay grid )
        ]

gridDisplay : Grid -> Html Msg
gridDisplay grid =
  let
    cellsEls = cells grid
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
      , navigationEvents
      ]
      ( cellsEls ++ dividers )

displayCell : CellValue -> (Number, Number) -> List ( Html Msg )
displayCell cellValue (xNum, yNum) =
  case cellValue of
    Empty ->
      cellHtml "" (xNum, yNum) False False
    Input num ->
      cellHtml ( numberAsString num ) (xNum, yNum) False False
    Fixed num ->
      cellHtml ( numberAsString num ) (xNum, yNum) True False

cellHtml : String -> (Number, Number) -> Bool -> Bool -> List ( Html Msg )
cellHtml contents (xNum, yNum) fixed selected =
  let
    xi = numberAsIndex xNum
    yi = numberAsIndex yNum
    xTextOffset = 22
    yTextOffset = 29
    fillColour =
      if fixed then
        "#cccccc"
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

cells : Grid -> List ( Html Msg )
cells grid =
  flattenList
    [ displayCell ( getCell (One, One) grid ) (One, One)
    , displayCell ( getCell (Two, One) grid ) (Two, One)
    , displayCell ( getCell (Three, One) grid ) (Three, One)
    , displayCell ( getCell (Four, One) grid ) (Four, One)
    , displayCell ( getCell (Five, One) grid ) (Five, One)
    , displayCell ( getCell (Six, One) grid ) (Six, One)
    , displayCell ( getCell (Seven, One) grid ) (Seven, One)
    , displayCell ( getCell (Eight, One) grid ) (Eight, One)
    , displayCell ( getCell (Nine, One) grid ) (Nine, One)
    , displayCell ( getCell (One, Two) grid ) (One, Two)
    , displayCell ( getCell (Two, Two) grid ) (Two, Two)
    , displayCell ( getCell (Three, Two) grid ) (Three, Two)
    , displayCell ( getCell (Four, Two) grid ) (Four, Two)
    , displayCell ( getCell (Five, Two) grid ) (Five, Two)
    , displayCell ( getCell (Six, Two) grid ) (Six, Two)
    , displayCell ( getCell (Seven, Two) grid ) (Seven, Two)
    , displayCell ( getCell (Eight, Two) grid ) (Eight, Two)
    , displayCell ( getCell (Nine, Two) grid ) (Nine, Two)
    , displayCell ( getCell (One, Three) grid ) (One, Three)
    , displayCell ( getCell (Two, Three) grid ) (Two, Three)
    , displayCell ( getCell (Three, Three) grid ) (Three, Three)
    , displayCell ( getCell (Four, Three) grid ) (Four, Three)
    , displayCell ( getCell (Five, Three) grid ) (Five, Three)
    , displayCell ( getCell (Six, Three) grid ) (Six, Three)
    , displayCell ( getCell (Seven, Three) grid ) (Seven, Three)
    , displayCell ( getCell (Eight, Three) grid ) (Eight, Three)
    , displayCell ( getCell (Nine, Three) grid ) (Nine, Three)
    , displayCell ( getCell (One, Four) grid ) (One, Four)
    , displayCell ( getCell (Two, Four) grid ) (Two, Four)
    , displayCell ( getCell (Three, Four) grid ) (Three, Four)
    , displayCell ( getCell (Four, Four) grid ) (Four, Four)
    , displayCell ( getCell (Five, Four) grid ) (Five, Four)
    , displayCell ( getCell (Six, Four) grid ) (Six, Four)
    , displayCell ( getCell (Seven, Four) grid ) (Seven, Four)
    , displayCell ( getCell (Eight, Four) grid ) (Eight, Four)
    , displayCell ( getCell (Nine, Four) grid ) (Nine, Four)
    , displayCell ( getCell (One, Five) grid ) (One, Five)
    , displayCell ( getCell (Two, Five) grid ) (Two, Five)
    , displayCell ( getCell (Three, Five) grid ) (Three, Five)
    , displayCell ( getCell (Four, Five) grid ) (Four, Five)
    , displayCell ( getCell (Five, Five) grid ) (Five, Five)
    , displayCell ( getCell (Six, Five) grid ) (Six, Five)
    , displayCell ( getCell (Seven, Five) grid ) (Seven, Five)
    , displayCell ( getCell (Eight, Five) grid ) (Eight, Five)
    , displayCell ( getCell (Nine, Five) grid ) (Nine, Five)
    , displayCell ( getCell (One, Six) grid ) (One, Six)
    , displayCell ( getCell (Two, Six) grid ) (Two, Six)
    , displayCell ( getCell (Three, Six) grid ) (Three, Six)
    , displayCell ( getCell (Four, Six) grid ) (Four, Six)
    , displayCell ( getCell (Five, Six) grid ) (Five, Six)
    , displayCell ( getCell (Six, Six) grid ) (Six, Six)
    , displayCell ( getCell (Seven, Six) grid ) (Seven, Six)
    , displayCell ( getCell (Eight, Six) grid ) (Eight, Six)
    , displayCell ( getCell (Nine, Six) grid ) (Nine, Six)
    , displayCell ( getCell (One, Seven) grid ) (One, Seven)
    , displayCell ( getCell (Two, Seven) grid ) (Two, Seven)
    , displayCell ( getCell (Three, Seven) grid ) (Three, Seven)
    , displayCell ( getCell (Four, Seven) grid ) (Four, Seven)
    , displayCell ( getCell (Five, Seven) grid ) (Five, Seven)
    , displayCell ( getCell (Six, Seven) grid ) (Six, Seven)
    , displayCell ( getCell (Seven, Seven) grid ) (Seven, Seven)
    , displayCell ( getCell (Eight, Seven) grid ) (Eight, Seven)
    , displayCell ( getCell (Nine, Seven) grid ) (Nine, Seven)
    , displayCell ( getCell (One, Eight) grid ) (One, Eight)
    , displayCell ( getCell (Two, Eight) grid ) (Two, Eight)
    , displayCell ( getCell (Three, Eight) grid ) (Three, Eight)
    , displayCell ( getCell (Four, Eight) grid ) (Four, Eight)
    , displayCell ( getCell (Five, Eight) grid ) (Five, Eight)
    , displayCell ( getCell (Six, Eight) grid ) (Six, Eight)
    , displayCell ( getCell (Seven, Eight) grid ) (Seven, Eight)
    , displayCell ( getCell (Eight, Eight) grid ) (Eight, Eight)
    , displayCell ( getCell (Nine, Eight) grid ) (Nine, Eight)
    , displayCell ( getCell (One, Nine) grid ) (One, Nine)
    , displayCell ( getCell (Two, Nine) grid ) (Two, Nine)
    , displayCell ( getCell (Three, Nine) grid ) (Three, Nine)
    , displayCell ( getCell (Four, Nine) grid ) (Four, Nine)
    , displayCell ( getCell (Five, Nine) grid ) (Five, Nine)
    , displayCell ( getCell (Six, Nine) grid ) (Six, Nine)
    , displayCell ( getCell (Seven, Nine) grid ) (Seven, Nine)
    , displayCell ( getCell (Eight, Nine) grid ) (Eight, Nine)
    , displayCell ( getCell (Nine, Nine) grid ) (Nine, Nine)
    ]
