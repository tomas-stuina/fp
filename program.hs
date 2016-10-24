module Program1
where

type Board = [Char]

data Move = Move {
    x :: Int
  , y :: Int
  , t :: Char
} deriving Show

trimString :: String -> String
trimString ('\"':t) = reverse $ trimString $ reverse t
trimString h = h

readDigit :: String -> Int
readDigit ('0':t) = 0
readDigit ('1':t) = 1
readDigit ('2':t) = 2
readDigit _ = error "Negalima koordinates reiksme"

readSymbol :: String -> Char
readSymbol (h:t)
  | h == 'x' || h == 'X' = 'x'
  | h == 'o' || h == 'O' = 'o'
  | otherwise = ' '

readValue :: String -> String  -> (String, String)
readValue [] [] = ([], [])
readValue [] b = (b, [])
readValue (h:t) b 
  | h == ']' = (b, t)
  | h == ';' = (b, t)
  | h == ' ' || h == '\"' = readValue t b
  | otherwise = readValue t (b ++ [h])

readMmapValue :: String -> String  -> (String, String, String)
readMmapValue exp b =
  let
    (key, t) = readValue exp []
    (val, rest) = readValue t []
  in
    (key, val, rest)

readMmap :: String -> (Move, String)
readMmap [] = (Move (-1) (-1) (' '), [])
readMmap ('m':'[':t) =
  let
    (keyX, valX, restX) = readMmapValue t []
    (keyY, valY, restY) = readMmapValue restX []
    (keyT, valT, restT) = readMmapValue restY []
  in
    (Move (readDigit valX) (readDigit valY) (readSymbol valT), restT)
readMmap (h:s) = readMmap s

readMListItems :: String -> [Move] -> [Move]
readMListItems (']':t) moves = moves
readMListItems exp moves = readMListItems rest moves ++ [m]
  where (m, rest) = readMmap exp

readMList :: String -> [Move]
readMList ('l':'[':t) = reverse (readMListItems t [])
readMList _ = error "Israiskoje nera saraso"

createBoard :: Char -> Board
createBoard t =
  let
    row = [t,t,t]
    emptyBoard = row ++ row ++ row
  in
    emptyBoard
    
replaceMoveType:: Int -> Char -> Board -> Board
replaceMoveType i v (h:t)
  | i == 0 = v:t
  | otherwise = h:replaceMoveType (i-1) v t

makeMove :: Move -> Board -> Board
makeMove move board = 
  let
    (Move x y t) = move
    coords = (x * 3) + y
  in
    replaceMoveType coords t board
    
processMoves :: [Move] -> Board -> Maybe Char
processMoves [] [] = Nothing
processMoves [] b = Nothing
processMoves (h:l) b =
  let
    (Move x y t) = h
    board = makeMove h b
    foundWin = hasWon t board
  in
    case foundWin of
      True -> Just t
      False -> processMoves l board;
  
fullRow :: Char -> Board -> Bool
fullRow t [] = False
fullRow t (a:b:c:s)
  | (a == b && b == c && c == t) = True
  | otherwise = fullRow t s
  
hasWon :: Char -> Board -> Bool
hasWon m board =
  let
    fullRows = fullRow m board
    vertical [a1,b1,c1,a2,b2,c2,a3,b3,c3] = fullRow m [a1,a2,a3,b1,b2,b3,c1,c2,c3]
    diagonal [a1,_,b1,_,c2,_,b3,_,a3] = fullRow m [a1,c2,a3,b1,c2,b3]
  in
    fullRows || diagonal board || vertical board
