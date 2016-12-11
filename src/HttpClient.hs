module HttpClient where

import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.Header
import qualified Data.ByteString.Char8 as Byte
import Data.ByteString.Lazy.Char8 as C
import Game
import MExp
import Data.Maybe

getUrl gameId player = "http://tictactoe.homedir.eu/game/" ++ gameId ++ "/player/" ++ player

play :: String -> String -> IO()
play gameId player = do
  if (player == "1")
  then
    do
      attack gameId
  else 
      defend gameId


attack :: String -> IO()
attack gameId = do
  let url = getUrl gameId "1"
  post url "x" []

defend :: String -> IO()
defend gameId = do
  let url = getUrl gameId "2"
  get url "o" []

get :: String -> String -> [Move] -> IO()
get url player moves = do
  let checkWinner = winner moves
  if checkWinner == Nothing
    then
      do
      manager <- newManager defaultManagerSettings
      initialRequest <- parseUrlThrow $ url
      let request = initialRequest { 
      method = Byte.pack "GET",
      requestHeaders = [(hContentType, Byte.pack "application/m-expr"),(hAccept, Byte.pack "application/m-expr")]
      }
      response <- httpLbs request manager
      Prelude.putStrLn ("GET")
      let movesFromOponent = getMovesFromMExp (unpack $ responseBody response)
      let (oponent:t) = Prelude.reverse movesFromOponent
      Prelude.putStrLn ( show $ oponent )
      Prelude.putStrLn ("")
      post url player movesFromOponent
  else
    Prelude.putStrLn ( fromJust checkWinner )

  
post :: String -> String -> [Move] -> IO()
post url player moves = do
  let checkWinner = winner moves
  if checkWinner == Nothing
    then
      do
      manager <- newManager defaultManagerSettings
      initialRequest <- parseUrlThrow $ url
      let playerMove = miniMax player moves
      let movesToSend = moves ++ [playerMove]
      let movesToSendExp = movesToMExp movesToSend
      Prelude.putStrLn ("POST")
      Prelude.putStrLn ( show $ playerMove )
      let request = initialRequest { 
      method = Byte.pack "POST",
      requestHeaders = [(hContentType, Byte.pack "application/m-expr"),(hAccept, Byte.pack "application/m-expr")],
      requestBody = RequestBodyLBS $ C.pack (movesToSendExp)
      }
      response <- httpLbs request manager
      get url player movesToSend
  else
    Prelude.putStrLn ( fromJust checkWinner )