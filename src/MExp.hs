module MExp where

import Data.Char


message :: String
message = "l[m[\"x\"; 2; \"y\";  1;   \"v\";  \"x\"];   m[\"x\";   0;  \"y\";   0;   \"v\";  \"o\"]; m[\"x\"; 2;   \"y\";   0; \"v\"; \"x\"]; m[\"x\";   1; \"y\";   1; \"v\";  \"o\"]; m[\"x\"; 1;   \"y\";  0;   \"v\";  \"x\"];  m[\"x\";   1; \"y\"; 2; \"v\"; \"o\"]]"

data Value = Value {
    valueType :: Char
  , intValue :: Int
  , charValue :: Char
  , stringValue :: String
  , listValue :: [Value]
  , mapValue :: [Value]
} deriving Show

emptyValue :: Value
emptyValue = Value ' ' 0 ' ' "" [] []

parseIntValue :: String -> Value
parseIntValue [] = (Value 'i' 0 ' ' "" [] [])
parseIntValue result =
    let 
        intValue = read result :: Int
    in
        (Value 'd' intValue ' ' "" [] [])

setIntValue :: String -> String -> (String, Value)
setIntValue [] [] = ([], emptyValue)
setIntValue [] result = ([], parseIntValue (reverse result))
setIntValue (h:t) result 
    | isDigit h = setIntValue t (h:result)
    | otherwise = ((h:t), parseIntValue (reverse result))
        

setStringValue :: String -> String -> (String, Value)
setStringValue ('"':t) result = (t, (Value 's' 0 ' ' (reverse result) [] []))
setStringValue (h:t) result = setStringValue t (h:result)

addListItem :: String -> Value -> (String, Value)
addListItem expression listValue = 
    let 
        (Value vT iV cV sV lV mV) = listValue
        (tail, valueToAdd) = parseInner expression emptyValue
    in
        (tail, (Value 'l' iV cV sV (lV ++ [valueToAdd]) mV))

parseList :: String -> Value -> (String, Value)
parseList "" value = ("", value)
parseList (h:t) value
    | h == '[' || h == ';' || h == ' ' = parseList t value
    | h == ']' = (t, value)
    | otherwise = 
        let 
            (tail, listValue) = addListItem (h:t) value
        in 
            parseList tail listValue


addMapItem :: String -> Value -> (String, Value)
addMapItem expression mapValue = 
    let 
        (Value vT iV cV sV lV mV) = mapValue
        (tail, valueToAdd) = parseInner expression emptyValue
    in
        (tail, (Value 'm' iV cV sV lV (mV ++ [valueToAdd])))

parseMap :: String -> Value -> (String, Value)
parseMap "" value = ("", value)
parseMap (h:t) value
    | h == '[' || h == ';' || h == ' ' = parseMap t value
    | h == ']' = (t, value)
    | otherwise = 
        let 
            (tail, mapValue) = addMapItem (h:t) value
        in 
            parseMap tail mapValue

parseInner :: String -> Value -> (String, Value)
parseInner (h:t) value 
    | h == 'l' = parseList t value
    | h == 'm' = parseMap t value
    | h == '"' = setStringValue t ""
    | isDigit h = setIntValue (h:t) ""
    | otherwise = ("", value)

parse :: String -> Value
parse expression =
    let 
        (tail, value) = parseInner expression emptyValue
    in
        value

applySemicolon :: [Value] -> String
applySemicolon [] = ""
applySemicolon values = "; "


listToMExpString :: [Value] -> String -> String
listToMExpString [] result = result
listToMExpString (h:t) result = 
    let
        item = toString h result
        semicolon = applySemicolon t
    in
        listToMExpString t (item ++ semicolon)

toString :: Value -> String -> String
toString (Value vT iV cV sV lV mV) result
    | vT == 'l' = (listToMExpString lV (result ++ "l[")) ++ "]"
    | vT == 'm' = (listToMExpString mV (result ++ "m[")) ++ "]"
    | vT == 's' = result ++ "\"" ++ sV ++ "\""
    | vT == 'd' = (result ++ (show iV))
    | vT == ' ' = result

getCollection :: Value -> [Value]
getCollection (Value vT iV cV sV lV mV)
    | vT == 'l' = lV
    | vT == 'm' = mV
    | otherwise = []

getInt :: Value -> Maybe Int
getInt (Value vT iV cV sV lV mV)
    | vT == 'd' = Just iV
    | otherwise = Nothing

getString :: Value -> Maybe String
getString (Value vT iV cV sV lV mV)
    | vT == 's' = Just sV
    | otherwise = Nothing

    