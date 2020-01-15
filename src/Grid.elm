module Grid exposing
  ( Number (..), Grid, CellValue (..), Cells
  , emptyGrid, getCell, getRow, getCol, getSquare, cellInSquare, setCell, clearCell, puzzle
  , gridAsString, numberAsString, numberAsIndex, numberFromIndex, numberMod
  , cellBySquareAndCellIndices, getCellSquare, isFilled
  )

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

-- Grid is exposed but not its constructor - Array is internal implementation detail
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

numberFromIndex : Int -> Result String Number
numberFromIndex i =
  case i of
    0 -> Ok One
    1 -> Ok Two
    2 -> Ok Three
    3 -> Ok Four
    4 -> Ok Five
    5 -> Ok Six
    6 -> Ok Seven
    7 -> Ok Eight
    8 -> Ok Nine
    _ -> Err ( "Invalid sudoku number " ++ ( String.fromInt i ) ++ ", needed 0 - 8" )

numberAsString : Number -> String
numberAsString num =
  case num of
    One -> "1"
    Two -> "2"
    Three -> "3"
    Four -> "4"
    Five -> "5"
    Six -> "6"
    Seven -> "7"
    Eight -> "8"
    Nine -> "9"

numberMod : Number -> Int -> Number
numberMod num d =
  let
    newIndex = modBy 9 ( ( numberAsIndex num ) + d )
  in
    Result.withDefault One ( numberFromIndex newIndex )

cellBySquareAndCellIndices : Number -> Number -> (Number, Number)
cellBySquareAndCellIndices squareNum cellNum =
  let
    squareIndex = numberAsIndex squareNum
    cellIndex = numberAsIndex cellNum
    xi = ( 3 * ( modBy 3 squareIndex ) ) + ( modBy 3 cellIndex )
    yi = ( 3 * ( squareIndex // 3 ) ) + ( cellIndex // 3 )
    xResult = numberFromIndex xi
    yResult = numberFromIndex yi
    cellResult = Result.map2 ( \x y -> (x, y) ) xResult yResult
  in
    case cellResult of
      Err str ->
        (One, One)
      Ok cell ->
        cell

-- safeX follows `Array.get` because we know statically the underlying Array will have 9 elements
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

updateCell : ( Number, Number ) -> CellValue -> Grid -> Result String Grid
updateCell (x, y) cellValue grid =
  let
    xi = numberAsIndex x
    yi = numberAsIndex y
    rows =
      case grid of
        Grid rs -> rs
    currentValue = getCell ( x, y ) grid
    currentRow = safeRow ( Array.get yi rows )
    newRow = Array.set xi cellValue currentRow
    newGrid = Array.set yi newRow rows
  in
    case currentValue of
      Fixed _ ->
        Err "Cannot update fixed cell"
      _ ->
        Ok ( Grid newGrid )

-- exposed functions

emptyGrid : Grid
emptyGrid =
  let
    row = Array.repeat 9 ()
    rows = Array.map (\_ -> Array.repeat 9 Empty) row
  in
    Grid rows

puzzle : List ((Number, Number), Number) -> Result String Grid
puzzle initialNumbers =
  List.foldl
    ( \((x, y), value) result ->
      Result.andThen ( updateCell (x, y) ( Fixed value ) ) result
    )
    ( Ok emptyGrid )
    initialNumbers

getCell : ( Number, Number ) -> Grid -> CellValue
getCell (x, y) ( Grid rows ) =
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
getSquare squareNum grid =
  let
    (x0, y0) =
      case squareNum of
        One ->
          ( One, One )
        Two ->
          ( Four, One )
        Three ->
          ( Seven, One )
        Four ->
          ( One, Four )
        Five ->
          ( Four, Four )
        Six ->
          ( Seven, Four )
        Seven ->
          ( One, Seven )
        Eight ->
          ( Four, Seven )
        Nine ->
          ( Seven, Seven )
  in
    cells
      ( getCell ( numberMod x0 0, numberMod y0 0 ) grid )
      ( getCell ( numberMod x0 1, numberMod y0 0 ) grid )
      ( getCell ( numberMod x0 2, numberMod y0 0 ) grid )
      ( getCell ( numberMod x0 0, numberMod y0 1 ) grid )
      ( getCell ( numberMod x0 1, numberMod y0 1 ) grid )
      ( getCell ( numberMod x0 2, numberMod y0 1 ) grid )
      ( getCell ( numberMod x0 0, numberMod y0 2 ) grid )
      ( getCell ( numberMod x0 1, numberMod y0 2 ) grid )
      ( getCell ( numberMod x0 2, numberMod y0 2 ) grid )

cellInSquare : Number -> (Number, Number) -> Bool
cellInSquare squareNum (xNum, yNum) =
  let
    x = numberAsIndex xNum
    y = numberAsIndex yNum
    squareIndex = numberAsIndex squareNum
    xMatches = ( modBy 3 squareIndex ) == ( x // 3 )
    yMatches = ( squareIndex // 3 ) == ( y // 3 )
  in
    xMatches && yMatches

getCellSquare : (Number, Number) -> Number
getCellSquare (xNum, yNum) =
  let
    xi = numberAsIndex xNum
    yi = numberAsIndex yNum
    result = numberFromIndex ( ( xi // 3 ) + ( 3 * ( yi // 3 ) ) )
  in
    case result of
      Ok num ->
        num
      Err message ->
        One

setCell : ( Number, Number ) -> Number -> Grid -> Result String Grid
setCell (x, y) value grid =
  updateCell (x, y) ( Input value ) grid

clearCell : ( Number, Number ) -> Grid -> Result String Grid
clearCell (x, y) grid =
  updateCell (x, y) Empty grid

isFilled : Grid -> Bool
isFilled grid =
  case grid of
    Grid arrOfCells ->
      Array.foldl
        (\row full ->
          Array.foldl
            (\cellValue rowFull ->
              case cellValue of
                Empty ->
                  False
                _ ->
                  rowFull && full
            )
            True
            row
        )
        True
        arrOfCells

-- representations

cellAsString : CellValue -> String
cellAsString cell =
  case cell of
    Empty ->
      " - "
    Fixed num ->
      "[" ++ ( numberAsString num ) ++ "]"
    Input num ->
      " " ++ ( numberAsString num ) ++ " "

cellStr : (Number, Number) -> Grid -> String
cellStr cell grid =
  cellAsString ( getCell cell grid )

gridAsString : Grid -> String
gridAsString grid =
  ( cellStr (One, One) grid ) ++ ( cellStr (Two, One) grid ) ++ ( cellStr (Three, One) grid ) ++ "|" ++
    ( cellStr (Four, One) grid ) ++ ( cellStr (Five, One) grid ) ++ ( cellStr (Six, One) grid ) ++ "|" ++
    ( cellStr (Seven, One) grid ) ++ ( cellStr (Eight, One) grid ) ++ ( cellStr (Nine, One) grid ) ++
  "//" ++
  ( cellStr (One, Two) grid ) ++ ( cellStr (Two, Two) grid ) ++ ( cellStr (Three, Two) grid ) ++ "|" ++
    ( cellStr (Four, Two) grid ) ++ ( cellStr (Five, Two) grid ) ++ ( cellStr (Six, Two) grid ) ++ "|" ++
    ( cellStr (Seven, Two) grid ) ++ ( cellStr (Eight, Two) grid ) ++ ( cellStr (Nine, Two) grid ) ++
  "//" ++
  ( cellStr (One, Three) grid ) ++ ( cellStr (Two, Three) grid ) ++ ( cellStr (Three, Three) grid ) ++ "|" ++
    ( cellStr (Four, Three) grid ) ++ ( cellStr (Five, Three) grid ) ++ ( cellStr (Six, Three) grid ) ++ "|" ++
    ( cellStr (Seven, Three) grid ) ++ ( cellStr (Eight, Three) grid ) ++ ( cellStr (Nine, Three) grid ) ++
  "//" ++
  ( cellStr (One, Four) grid ) ++ ( cellStr (Two, Four) grid ) ++ ( cellStr (Three, Four) grid ) ++ "|" ++
    ( cellStr (Four, Four) grid ) ++ ( cellStr (Five, Four) grid ) ++ ( cellStr (Six, Four) grid ) ++ "|" ++
    ( cellStr (Seven, Four) grid ) ++ ( cellStr (Eight, Four) grid ) ++ ( cellStr (Nine, Four) grid ) ++
  "//" ++
  ( cellStr (One, Five) grid ) ++ ( cellStr (Two, Five) grid ) ++ ( cellStr (Three, Five) grid ) ++ "|" ++
    ( cellStr (Four, Five) grid ) ++ ( cellStr (Five, Five) grid ) ++ ( cellStr (Six, Five) grid ) ++ "|" ++
    ( cellStr (Seven, Five) grid ) ++ ( cellStr (Eight, Five) grid ) ++ ( cellStr (Nine, Five) grid ) ++
  "//" ++
  ( cellStr (One, Six) grid ) ++ ( cellStr (Two, Six) grid ) ++ ( cellStr (Three, Six) grid ) ++ "|" ++
    ( cellStr (Four, Six) grid ) ++ ( cellStr (Five, Six) grid ) ++ ( cellStr (Six, Six) grid ) ++ "|" ++
    ( cellStr (Seven, Six) grid ) ++ ( cellStr (Eight, Six) grid ) ++ ( cellStr (Nine, Six) grid ) ++
  "//" ++
  ( cellStr (One, Seven) grid ) ++ ( cellStr (Two, Seven) grid ) ++ ( cellStr (Three, Seven) grid ) ++ "|" ++
    ( cellStr (Four, Seven) grid ) ++ ( cellStr (Five, Seven) grid ) ++ ( cellStr (Six, Seven) grid ) ++ "|" ++
    ( cellStr (Seven, Seven) grid ) ++ ( cellStr (Eight, Seven) grid ) ++ ( cellStr (Nine, Seven) grid ) ++
  "//" ++
  ( cellStr (One, Eight) grid ) ++ ( cellStr (Two, Eight) grid ) ++ ( cellStr (Three, Eight) grid ) ++ "|" ++
    ( cellStr (Four, Eight) grid ) ++ ( cellStr (Five, Eight) grid ) ++ ( cellStr (Six, Eight) grid ) ++ "|" ++
    ( cellStr (Seven, Eight) grid ) ++ ( cellStr (Eight, Eight) grid ) ++ ( cellStr (Nine, Eight) grid ) ++
  "//" ++
  ( cellStr (One, Nine) grid ) ++ ( cellStr (Two, Nine) grid ) ++ ( cellStr (Three, Nine) grid ) ++ "|" ++
    ( cellStr (Four, Nine) grid ) ++ ( cellStr (Five, Nine) grid ) ++ ( cellStr (Six, Nine) grid ) ++ "|" ++
    ( cellStr (Seven, Nine) grid ) ++ ( cellStr (Eight, Nine) grid ) ++ ( cellStr (Nine, Nine) grid )
