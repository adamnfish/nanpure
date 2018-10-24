module Models exposing (Number (..), Grid, CellValue (..), emptyGrid, getCell, getRow, getCol, getSquare, updateCell)

import Array exposing (Array)


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

-- Grid is expose but not its constructor - Array is internal implementation detail
type Grid =
  Grid ( Array ( Array CellValue ) )

type alias Cells =
  { c1 : CellValue
  , c2 : CellValue
  , c3 : CellValue
  , c4 : CellValue
  , c5 : CellValue
  , c6 : CellValue
  , c7 : CellValue
  , c8 : CellValue
  , c9 : CellValue
  }

-- internal functions

cells : CellValue -> CellValue -> CellValue -> CellValue -> CellValue -> CellValue -> CellValue -> CellValue -> CellValue -> Cells
cells c1 c2 c3 c4 c5 c6 c7 c8 c9 =
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

numberAsIndex : Number -> Int
numberAsIndex num =
  case num of
    One -> 0
    Two -> 1
    Three -> 2
    Four -> 3
    Five -> 4
    Six -> 5
    Seven -> 6
    Eight -> 7
    Nine -> 8

safeRow : Maybe ( Array CellValue ) -> Array CellValue
safeRow mr =
  case mr of
    Nothing ->
      Array.repeat 9 Empty
    Just r ->
      r
safeCell : Maybe CellValue -> CellValue
safeCell mcv =
  case mcv of
    Nothing ->
      Empty
    Just cv ->
      cv

arrayToCells : Array CellValue -> Cells
arrayToCells cellValues =
  cells
    ( safeCell ( Array.get ( numberAsIndex One ) cellValues ) )
    ( safeCell ( Array.get ( numberAsIndex Two ) cellValues ) )
    ( safeCell ( Array.get ( numberAsIndex Three ) cellValues ) )
    ( safeCell ( Array.get ( numberAsIndex Four ) cellValues ) )
    ( safeCell ( Array.get ( numberAsIndex Five ) cellValues ) )
    ( safeCell ( Array.get ( numberAsIndex Six ) cellValues ) )
    ( safeCell ( Array.get ( numberAsIndex Seven ) cellValues ) )
    ( safeCell ( Array.get ( numberAsIndex Eight ) cellValues ) )
    ( safeCell ( Array.get ( numberAsIndex Nine ) cellValues ) )

-- exposed functions

emptyGrid : Grid
emptyGrid =
  let
    row = Array.repeat 9 ()
    rows = Array.map (\_ -> Array.repeat 9 Empty) row
  in
    Grid rows

getCell : ( Number, Number ) -> Grid -> CellValue
getCell ( x,  y ) ( Grid rows ) =
  let
    xi = numberAsIndex x
    yi = numberAsIndex y
    maybeCell =
      ( Array.get yi rows )
        |> Maybe.andThen ( Array.get xi )
  in
    safeCell maybeCell

getRow : Number -> Grid -> Cells
getRow y ( Grid rows ) =
  let
    yi = numberAsIndex y
    row = safeRow ( Array.get yi rows )
  in
    arrayToCells row

getCol : Number -> Grid -> Cells
getCol x ( Grid rows ) =
  let
    xi = numberAsIndex x
    colMaybes = Array.map ( Array.get xi ) rows
    col = Array.map safeCell colMaybes
  in
    arrayToCells col

getSquare : Number -> Grid -> Cells
getSquare i ( Grid rows ) =
--  let
--  in
--    arrayToCells square
  cells Empty Empty Empty Empty Empty Empty Empty Empty Empty

updateCell : ( Number, Number ) -> Maybe Number -> Grid -> Result () Grid
updateCell ( x, y ) maybeValue grid =
  let
    xi = numberAsIndex x
    yi = numberAsIndex y
    currentValue = getCell ( x, y ) grid
    rows =
      case grid of
        Grid rs -> rs
    currentRow = safeRow ( Array.get yi rows )
    newCellValue =
      case maybeValue of
        Just n ->
          Input n
        Nothing ->
          Empty
    newRow = Array.set xi newCellValue currentRow
    newGrid = Array.set yi newRow rows
  in
    case currentValue of
      Fixed _ ->
        Err ()
      _ ->
        Ok ( Grid newGrid )
