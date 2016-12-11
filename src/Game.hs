module Game where

import MExp
import Data.Char
import Data.Maybe
import Data.Function (on)
import Data.List


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

oponent :: String -> String
oponent "x" = "o"
oponent _ = "x"


getPlayer :: Bool -> String
getPlayer player
    | player = "x"
    | otherwise = "o"

getPlayerState :: String -> Bool
getPlayerState player
    | player == "x" = True
    | otherwise = False

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


getMovesFromMExp :: String -> [Move]
getMovesFromMExp [] = []
getMovesFromMExp moveMExp = mExpToMoves moveMExp



miniMax :: String -> [Move] -> Move
miniMax player [] = (Move 0 0 player)
miniMax player moves = 
        let
            availableMoves = getAvailableMoves player moves
            mapped = map (\move -> ((minPlay (oponent player) player (makeMove player move moves) (availableMoves \\ [move])), move)) availableMoves
            (score, move) = maximumBy (compare `on` fst) mapped
        in
            move


minPlay :: String -> String-> [Move] -> [Move] -> Int
minPlay player team moves availableMoves
    | (winner moves) == Just team = 1
    | (winner moves) == Just (oponent team) = -1
    | (winner moves) == Just "0" = 0
    | otherwise =
        let
            mapped = map (\move -> maxPlay (oponent player) team (makeMove player move moves) (availableMoves \\ [move])) availableMoves
        in
            minimum mapped

maxPlay :: String -> String -> [Move] -> [Move] -> Int
maxPlay player team moves availableMoves
    | (winner moves) == Just team = 1
    | (winner moves) == Just (oponent team) = -1
    | (winner moves) == Just "0" = 0
    | otherwise =
        let
            mapped = map (\move -> minPlay (oponent player) team (makeMove player move moves) (availableMoves \\ [move])) availableMoves
        in
            maximum mapped


makeMove player move moves = let (Move x y t) = move in moves ++ [(Move x y player)]

getAvailableMoves player moves = loop player 0 moves []

loop :: String -> Int -> [Move] -> [Move] -> [Move]
loop player pos moves available
    | pos < 9 && (moveExistAtPos pos moves) == False  = 
        let
            (x, y) = posToCoords pos
        in
            loop player (pos + 1) moves (available ++ [(Move x y player)])
    | pos < 9  = (loop player (pos + 1) moves available)
    | otherwise = available