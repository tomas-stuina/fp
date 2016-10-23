module Program1
where

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
