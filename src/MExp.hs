module MExp where

import Data.Char

data Value = Value {
    valueType :: Char
  , intValue :: Int
  , charValue :: Char
  , stringValue :: String
  , listValue :: [Value]
  , mapValue :: [(Value, Value)]
} deriving (Eq,Ord,Show)

emptyValue :: Value
emptyValue = Value ' ' 0 ' ' "" [] []

intToValue :: Int -> Value
intToValue number = (Value 'd' number ' ' "" [] [])

stringToValue :: String -> Value
stringToValue text = (Value 's' 0 ' ' text [] [])

valuesListToValue :: [Value] -> Value
valuesListToValue valuesList = (Value 'l' 0 ' ' "" valuesList [])

valuesMapToValue :: [(Value, Value)] -> Value
valuesMapToValue valuesList = (Value 'm' 0 ' ' "" [] valuesList)

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


addMapItem :: Value -> [Value] -> Value
addMapItem map [] = map
addMapItem map (h:m:mapValues) = 
    let 
        (Value vT iV cV sV lV mV) = map
        increasedMap = (Value 'm' iV cV sV lV (mV ++ [(h,m)]))
    in
        addMapItem increasedMap mapValues

parseMap :: String -> Value -> (String, Value)
parseMap "" value = ("", value)
parseMap expression value =
        let 
            (tail, list) = parseList expression emptyValue
            (Value vT iV cV sV lV mV) = list
            mapValue = addMapItem value lV
        in 
            (tail, mapValue)

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

applySemicolonMap :: [(Value,Value)] -> String
applySemicolonMap [] = ""
applySemicolonMap values = "; "

mapToMExpString :: [(Value, Value)] -> String -> String
mapToMExpString [] result = result
mapToMExpString (h:t) result = 
    let
        (key, value) = h
        keyStr = toStringInner key result
        keyValueSplt = keyStr ++ "; "
        valueStr = toStringInner value keyValueSplt
        semicolon = applySemicolonMap t
        
    in
        mapToMExpString t (valueStr ++ semicolon)

listToMExpString :: [Value] -> String -> String
listToMExpString [] result = result
listToMExpString (h:t) result = 
    let
        item = toStringInner h result
        semicolon = applySemicolon t
    in
        listToMExpString t (item ++ semicolon)

toStringInner :: Value -> String -> String
toStringInner (Value vT iV cV sV lV mV) result
    | vT == 'l' = (listToMExpString lV (result ++ "l[")) ++ "]"
    | vT == 'm' = (mapToMExpString mV (result ++ "m[")) ++ "]"
    | vT == 's' = result ++ "\"" ++ sV ++ "\""
    | vT == 'd' = (result ++ (show iV))
    | vT == ' ' = result

valuesToString :: [Value] -> String -> String
valuesToString [] result = result 
valuesToString (h:t) result = valuesToString t ((toString h) ++ result) 

toString :: Value -> String
toString value = toStringInner value ""

getList :: Value -> [Value]
getList (Value vT iV cV sV lV mV)
    | vT == 'l' = lV
    | otherwise = []

getMap :: Value -> [(Value,Value)]
getMap (Value vT iV cV sV lV mV)
    | vT == 'm' = mV
    | otherwise = []

getFirstMapItemInner :: [(Value, Value)] -> (Value, Value)
getFirstMapItemInner [] = (emptyValue, emptyValue)
getFirstMapItemInner (first:tail) = first

getMapItem :: Value -> Value -> (Value,Value)
getMapItem keyToFind (Value vT iV cV sV lV mV)
    | vT == 'm' = getFirstMapItemInner (filter( \(key, value) -> key == keyToFind) mV)
    | otherwise = (emptyValue, emptyValue)

getInt :: Value -> Maybe Int
getInt (Value vT iV cV sV lV mV)
    | vT == 'd' = Just iV
    | otherwise = Nothing

getString :: Value -> Maybe String
getString (Value vT iV cV sV lV mV)
    | vT == 's' = Just sV
    | otherwise = Nothing

    