module HttpClient where

import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.Header
import qualified Data.ByteString.Char8 as Byte
import Data.ByteString.Lazy.Char8 as C

getUrl id = "http://httpbin.org/post"

get :: String -> IO()
get id = do
  manager <- newManager defaultManagerSettings
  initialRequest <- parseUrlThrow $ getUrl id
  let request = initialRequest { 
   method = Byte.pack "GET",
   requestHeaders = [(hContentType, Byte.pack "application/m-expr+list"),(hAccept, Byte.pack "application/m-expr+list")]
   }
  response <- httpLbs request manager
  Prelude.putStrLn $ "Status code: " ++ (show $ statusCode $ responseStatus response)
  Prelude.putStrLn ( unpack $ responseBody response)

  
post :: String -> String -> IO()
post id message = do
  manager <- newManager defaultManagerSettings
  initialRequest <- parseUrlThrow $ getUrl id
  let request = initialRequest { 
  method = Byte.pack "POST",
  requestHeaders = [(hContentType, Byte.pack "application/m-expr+list"),(hAccept, Byte.pack "application/m-expr+list")],
  requestBody = RequestBodyLBS $ C.pack (message)   
  }

  response <- httpLbs request manager
  Prelude.putStrLn $ "Status code: " ++ (show $ statusCode $ responseStatus response)
  Prelude.putStrLn ( unpack $ responseBody response)