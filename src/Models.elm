module Models exposing (Number, CellValue)


type Number
  = One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine

type CellValue
  = Empty
  | Fixed Number
  | Input Number

type Grid =
  Grid (
    ( CellValue, CellValue, CellValue, CellValue, CellValue, CellValue, CellValue, CellValue, CellValue ),
    ( CellValue, CellValue, CellValue, CellValue, CellValue, CellValue, CellValue, CellValue, CellValue ),
    ( CellValue, CellValue, CellValue, CellValue, CellValue, CellValue, CellValue, CellValue, CellValue ),
    ( CellValue, CellValue, CellValue, CellValue, CellValue, CellValue, CellValue, CellValue, CellValue ),
    ( CellValue, CellValue, CellValue, CellValue, CellValue, CellValue, CellValue, CellValue, CellValue ),
    ( CellValue, CellValue, CellValue, CellValue, CellValue, CellValue, CellValue, CellValue, CellValue ),
    ( CellValue, CellValue, CellValue, CellValue, CellValue, CellValue, CellValue, CellValue, CellValue ),
    ( CellValue, CellValue, CellValue, CellValue, CellValue, CellValue, CellValue, CellValue, CellValue ),
    ( CellValue, CellValue, CellValue, CellValue, CellValue, CellValue, CellValue, CellValue, CellValue )
  )

emptyGrid : Grid
emptyGrid =
  Grid (
    ( Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ),
    ( Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ),
    ( Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ),
    ( Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ),
    ( Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ),
    ( Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ),
    ( Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ),
    ( Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ),
    ( Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty )
  )

getCell : ( Number, Number ) -> Grid -> CellValue
getCell ( x,  y ) ( Grid ( r1, r2, r3, r4, r5, r6, r7, r8, r9 ) ) =
  let
    row = case x of
      One -> r1
      Two -> r2
      Three -> r3
      Four -> r4
      Five -> r5
      Six -> r6
      Seven -> r7
      Eight -> r8
      Nine -> r9
    ( c1, c2, c3, c4, c5, c6, c7, c8, c9 ) = row
  in
    case y of
      One -> c1
      Two -> c2
      Three -> c3
      Four -> c4
      Five -> c5
      Six -> c6
      Seven -> c7
      Eight -> c8
      Nine -> c9

getRow : Number -> Grid -> (CellValue, CellValue, CellValue, CellValue, CellValue, CellValue, CellValue, CellValue, CellValue)
getRow x ( Grid ( r1, r2, r3, r4, r5, r6, r7, r8, r9 ) ) =
  case x of
    One -> r1
    Two -> r2
    Three -> r3
    Four -> r4
    Five -> r5
    Six -> r6
    Seven -> r7
    Eight -> r8
    Nine -> r9

getCol : Number -> Grid -> (CellValue, CellValue, CellValue, CellValue, CellValue, CellValue, CellValue, CellValue, CellValue)
getCol y grid =
  ???

getSquare : Number -> Grid -> (CellValue, CellValue, CellValue, CellValue, CellValue, CellValue, CellValue, CellValue, CellValue)
getSquare i grid =
  ???

updateCell : ( Number, Number ) -> Number -> Grid -> Grid
updateCell ( x, y ) n grid =
  let
    (
      ( r1c1, r1c2, r1c3, r1c4, r1c5, r1c6, r1c7, r1c8, r1c9 ),
      ( r2c1, r2c2, r2c3, r2c4, r2c5, r2c6, r2c7, r2c8, r2c9 ),
      ( r3c1, r3c2, r3c3, r3c4, r3c5, r3c6, r3c7, r3c8, r3c9 ),
      ( r4c1, r4c2, r4c3, r4c4, r4c5, r4c6, r4c7, r4c8, r4c9 ),
      ( r5c1, r5c2, r5c3, r5c4, r5c5, r5c6, r5c7, r5c8, r5c9 ),
      ( r6c1, r6c2, r6c3, r6c4, r6c5, r6c6, r6c7, r6c8, r6c9 ),
      ( r7c1, r7c2, r7c3, r7c4, r7c5, r7c6, r7c7, r7c8, r7c9 ),
      ( r8c1, r8c2, r8c3, r8c4, r8c5, r8c6, r8c7, r8c8, r8c9 ),
      ( r9c1, r9c2, r9c3, r9c4, r9c5, r9c6, r9c7, r9c8, r9c9 ),
    ) = grid
  in
    ???