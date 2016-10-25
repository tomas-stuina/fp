module Program1
where

{-
Modulis skirtas apdoroti m-expression zinute, kurioje
uzkoduoti kryziuku-nuliuku ejimai.

Laimetojo nustatymui naudojama "winner :: String -> Maybe Char" funkcija.
-}

type Board = [Char]

data Move = Move {
    x :: Int
  , y :: Int
  , t :: Char
} deriving Show

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
  | h == ']' = (b, t) -- List arba map pabaiga
  | h == ';' = (b, t) -- Reksmiu skirtukas
  | h == ' ' || h == '\"' = readValue t b -- Jeigu reiksme String, praleidziam kabutes.
  | otherwise = readValue t (b ++ [h])

readMapMValue :: String -> String  -> (String, String, String)
readMapMValue exp b =
  let
    (key, t) = readValue exp []
    (val, rest) = readValue t []
  in
    (key, val, rest)

readMapM :: String -> (Move, String)
readMapM [] = (Move (-1) (-1) (' '), [])
readMapM ('m':'[':t) =
  let
    (keyX, valX, restX) = readMapMValue t []
    (keyY, valY, restY) = readMapMValue restX []
    (keyT, valT, restT) = readMapMValue restY []
  in
    (Move (readDigit valX) (readDigit valY) (readSymbol valT), restT)
readMapM (h:s) = readMapM s

readListMItems :: String -> [Move] -> [Move]
readListMItems (']':t) moves = moves
readListMItems exp moves = readListMItems rest moves ++ [m]
  where (m, rest) = readMapM exp

readListM :: String -> [Move]
readListM ('l':'[':t) = reverse (readListMItems t [])
readListM _ = error "Israiskoje nera saraso"

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
    coords = (x * 3) + y -- koordinates skaiciuojamos lyg dvimacio 3x3 masyvo
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
    fullRows = fullRow m board -- Patikrinamos horizontalios eilutes
    vertical [a1,b1,c1,a2,b2,c2,a3,b3,c3] = fullRow m [a1,a2,a3,b1,b2,b3,c1,c2,c3] -- Vertikalios paverciamos horizontaliomis ir patikrinamos.
    diagonal [a1,_,b1,_,c2,_,b3,_,a3] = fullRow m [a1,c2,a3,b1,c2,b3] -- Istrizaines paverciamos horizontaliomis ir patikrinamos.
  in
    fullRows || diagonal board || vertical board -- Tikriname ar zaidejas laimejo siuo ejimu

winner :: String -> Maybe Char
winner exp =
  let 
    moves = readListM exp -- Nuskaitomi ejimai
    board = createBoard ' ' -- Sukuriama lenta su tusciais simboliais
  in 
    processMoves moves board -- Ieskome laimetojo.
