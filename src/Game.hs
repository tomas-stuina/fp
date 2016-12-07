module Game where

import MExp
import Data.Char
import Data.Maybe


data Move = Move {
    x :: Int
  , y :: Int
  , m :: String
} deriving (Eq,Ord,Show)

emptyMove = (Move (-1) (-1) "")

valuesListToMoves :: [Value] -> [Move] -> [Move]
valuesListToMoves [] [] = []
valuesListToMoves [] moves = moves
valuesListToMoves (h:t) moves =
    let
        (xKey, xVal) = getMapItem (stringToValue "x") h
        (yKey, yVal) = getMapItem (stringToValue "y") h
        (vKey, vVal) = getMapItem (stringToValue "v") h
        x = fromJust $ getInt xVal
        y = fromJust $ getInt yVal
        v = fromJust $ getString vVal
    in
        valuesListToMoves t (moves ++ [(Move x y v)])

mExpToMoves :: String -> [Move]
mExpToMoves expression =
    let
        list = getList (parse expression)
        moves = valuesListToMoves list []
    in
        moves

movesToValuesList :: [Move] -> [Value] -> [Value]
movesToValuesList [] [] = []
movesToValuesList [] values = values
movesToValuesList (h:t) values =
    let
        (Move x y v) = h
        (xKey, xVal) = (stringToValue "x", intToValue x)
        (yKey, yVal) = (stringToValue "y", intToValue y)
        (vKey, vVal) = (stringToValue "v", stringToValue v)
        valuesMap = [(xKey, xVal), (yKey, yVal), (vKey, vVal)]
        map = valuesMapToValue valuesMap
    in
        movesToValuesList t (values ++ [map])

movesToMExp :: [Move] -> String
movesToMExp moves =
    let
        valuesList = valuesListToValue (movesToValuesList moves [])
    in
        toString valuesList

movesToMExpString :: [Move] -> String
movesToMExpString moves =
    let
        valuesList = movesToValuesList moves []
    in
        valuesToString valuesList ""

moveExist :: Int -> Int -> String -> [Move] -> Bool
moveExist x y t moves
    | ((filter( \(Move mX mY mT) -> mX == x && mY == y && mT == t) moves) /= [] ) = True
    | otherwise = False


moveExistAtCoords :: Int -> Int -> [Move] -> Bool
moveExistAtCoords x y moves
    | ((filter( \(Move mX mY mT) -> mX == x && mY == y) moves) /= [] ) = True
    | otherwise = False

moveExistAtPos :: Int -> [Move] -> Bool
moveExistAtPos pos moves =
    let
        (x, y) = posToCoords pos
    in
        moveExistAtCoords x y moves

countMoves :: String -> [Move] -> Int
countMoves t moves = length ( filter (\(Move mX mY mT) -> mT == t) moves)

getMoves :: String -> [Move] -> [Move]
getMoves t moves = filter (\(Move mX mY mT) -> mT == t) moves

replaceCharAtPos:: Int -> Char -> String -> String
replaceCharAtPos i v (h:t)
  | i == 0 = v:t
  | otherwise = h:replaceCharAtPos (i-1) v t

movesToStringInner :: [Move] -> String -> String
movesToStringInner [] expression = expression
movesToStringInner (move:moves) expression =
    let
        (Move mX mY mT) = move
        pos = coordsToPos (mX, mY)
        (h:t) = mT
        result = replaceCharAtPos pos h expression
    in
        movesToStringInner moves result

movesToString :: [Move] -> String
movesToString moves = movesToStringInner moves "000000000"


posToCoords :: Int -> (Int, Int)
posToCoords pos =
    let
        y = pos `div` 3
        x = pos `mod` 3
    in
        (x, y)

coordsToPos :: (Int, Int) -> Int
coordsToPos (x, y) = (y * 3) + x

getOposite :: String -> String
getOposite player
    | player == "x" = "o"
    | player == "o" = "x"
    | otherwise = "0"

getMultiplier :: String -> String -> Int
getMultiplier player oposite
    | player == oposite = 1
    | otherwise = (-1)

coalesceScore :: Int -> Int -> Int
coalesceScore this max
    | this > max = this
    | otherwise = max

coalesceMove :: Move -> Move -> Int -> Int -> (Move, Int) 
coalesceMove thisMove bestMove this max
    | this > max = (thisMove, this)
    | otherwise = (bestMove, max)

fullRow :: Char -> String -> Bool
fullRow t [] = False
fullRow t (a:b:c:s)
  | (a == b && b == c && c == t) = True
  | otherwise = fullRow t s
  
hasWon :: Char -> [Move] -> Bool
hasWon m moves =
  let
    board = movesToString moves
    horizontals = fullRow m board -- Patikrinamos horizontalios eilutes
    vertical [a1,b1,c1,a2,b2,c2,a3,b3,c3] = fullRow m [a1,a2,a3,b1,b2,b3,c1,c2,c3] -- Vertikalios paverciamos horizontaliomis ir patikrinamos.
    diagonal [a1,_,b1,_,c2,_,b3,_,a3] = fullRow m [a1,c2,a3,b1,c2,b3] -- Istrizaines paverciamos horizontaliomis ir patikrinamos.
  in
    horizontals || diagonal board || vertical board -- Tikriname ar zaidejas laimejo siuo ejimu

boardFull :: [Move] -> Bool
boardFull moves =
  let
    board = movesToString moves
  in
    ((filter( \(t) -> t == '0') board) == [] )


winner :: [Move] -> Maybe String
winner moves
    | hasWon 'x' moves = Just "x"
    | hasWon 'o' moves = Just "o"
    | boardFull moves = Just "0"
    | otherwise = Nothing

miniMaxBoard :: Int -> Int -> Int -> String -> String -> [Move] -> Int
miniMaxBoard pos maxScore multiplier player team moves
    | pos < 9 && (moveExistAtPos pos moves) == False =
        let
            (x, y) = posToCoords pos
            copyMoves = moves ++ [(Move x y player)]
            oposite = getOposite player
            score = multiplier * (miniMax copyMoves oposite team)
            calcMax = coalesceScore score maxScore
        in
            miniMaxBoard (pos + 1) multiplier calcMax player team moves

    | pos < 9 = miniMaxBoard (pos + 1) multiplier maxScore player team moves
    | otherwise = maxScore


miniMax :: [Move] -> String -> String -> Int
miniMax moves player team
    | (winner moves) == Nothing =
        let
            pos = 0
            score = (-1)
            multiplier = getMultiplier player team
            maxScore = (miniMaxBoard pos score multiplier player team moves)
        in
            (multiplier * maxScore)
    | (winner moves) == Just "0" = 0
    | (winner moves) == Just team = 1
    | otherwise = -1

makeMoveBoard :: Move -> Int -> Int -> String -> [Move] -> Move
makeMoveBoard move pos maxScore team moves
    | pos < 9 && (moveExistAtPos pos moves) == False =
        let
            (x, y) = posToCoords pos
            currentMove = (Move x y team)
            copyMoves = moves ++ [currentMove]
            oposite = getOposite team
            score = (miniMax copyMoves oposite team)
            (newMove, calcMax) = coalesceMove currentMove move score maxScore
        in
            makeMoveBoard newMove (pos + 1) calcMax team moves

    | pos < 9 = makeMoveBoard move (pos + 1) maxScore team moves
    | otherwise = move


makeMove :: [Move] -> String -> Move
makeMove moves player =
    let
        pos = 0
        score = (-1)
        newMove = emptyMove
        move = (makeMoveBoard newMove pos score player moves)
    in
        move


getMovesFromMExp :: String -> [Move]
getMovesFromMExp [] = []
getMovesFromMExp moveMExp = mExpToMoves moveMExp
    

test :: String
test = 
    let
        moves = [(Move 0 0 "x"),(Move 1 0 "o"),(Move 2 0 "x"),(Move 0 1 "o"),(Move 1 1 "x"),(Move 0 2 "o"),(Move 2 1 "x")]
        playerMove = makeMove moves "o"
    in
        show (playerMove)
