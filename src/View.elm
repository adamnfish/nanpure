module View exposing (view)

import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src, class)
import Svg exposing (svg)
import Svg.Attributes exposing (height, width, stroke, fill, x, y)

import Model exposing (Model)
import Grid exposing (Grid, Number (..), CellValue (..), getCell, numberAsString, numberAsIndex)
import Msg exposing (Msg (..))


view : Model -> Html Msg
view model =
    div [ class "sudogu--container" ]
        [ ( gridDisplay model.grid )
        ]

gridDisplay : Grid -> Html Msg
gridDisplay grid =
  Svg.svg
    [ height "450"
    , width "450"
    , stroke "black"
    ]
    ( flatten
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
    )

displayCell : CellValue -> (Number, Number) -> List ( Html Msg )
displayCell cellValue (xNum, yNum) =
  let
    xi = numberAsIndex xNum
    yi = numberAsIndex yNum
  in
    case cellValue of
      Empty ->
        [ Svg.rect
          [ height "50"
          , width "50"
          , stroke "black"
          , x ( String.fromInt ( 50 * xi ) )
          , y ( String.fromInt ( 50 * yi ) )
          , fill "white"
          ]
          []
        , Svg.text_
            [ x ( String.fromInt (50 * xi + 22) )
            , y ( String.fromInt (50 * yi + 29) )
            ]
            [ text "-" ]
        ]
      Input num ->
        [ Svg.rect
          [ height "50"
          , width "50"
          , stroke "black"
          , x ( String.fromInt ( 50 * xi ) )
          , y ( String.fromInt ( 50 * yi ) )
          , fill "white"
          ]
          []
        , Svg.text_
          [ x ( String.fromInt (50 * xi + 22) )
          , y ( String.fromInt (50 * yi + 29) )
          ]
          [ text (numberAsString num) ]
        ]
      Fixed num ->
        [ Svg.rect
          [ height "50"
          , width "50"
          , stroke "black"
          , x ( String.fromInt ( 50 * xi ) )
          , y ( String.fromInt ( 50 * yi ) )
          , fill "white"
          ]
          []
        , Svg.text_
          [ x ( String.fromInt (50 * xi + 22) )
          , y ( String.fromInt (50 * yi + 29) )
          ]
          [ text (numberAsString num) ]
        ]

flatten : List ( List a ) -> List a
flatten lls =
  List.foldr (++) [] lls
