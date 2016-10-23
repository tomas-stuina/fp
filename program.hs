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