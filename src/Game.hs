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

mExpToMove :: String -> Move
mExpToMove expression =
    let
        value = parse expression
        (xVal:yVal:tVal:rest) = getCollection value
        x = fromJust $ getInt xVal
        y = fromJust $ getInt yVal
        t = fromJust $ getString tVal
    in
        (Move x y t)

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

minMaxBoard :: Int -> Int -> Int -> String -> String -> [Move] -> Int
minMaxBoard pos maxScore multiplier player team moves
    | pos < 9 && (moveExistAtPos pos moves) == False =
        let
            (x, y) = posToCoords pos
            copyMoves = moves ++ [(Move x y player)]
            oposite = getOposite player
            score = multiplier * (minMax copyMoves oposite team)
            calcMax = coalesceScore score maxScore
        in
            minMaxBoard (pos + 1) multiplier calcMax player team moves

    | pos < 9 = minMaxBoard (pos + 1) multiplier maxScore player team moves
    | otherwise = maxScore


minMax :: [Move] -> String -> String -> Int
minMax moves player team
    | (winner moves) == Nothing =
        let
            pos = 0
            score = (-1)
            multiplier = getMultiplier player team
            maxScore = (minMaxBoard pos score multiplier player team moves)
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
            score = (minMax copyMoves oposite team)
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


test :: Int -> Move
test x = 
    let
        moves = [(Move 0 0 "x"), (Move 1 0 "o"),(Move 2 0 "x"),(Move 1 1 "o")]
    in
        makeMove moves "x"
