module Main where

import MExp
import HttpClient
import Game

main :: IO ()
main = do
    Prelude.putStrLn "Iveskite zaidimo id:"
    gameId <- getLine
    Prelude.putStrLn "Iveskite zaidejo numeri:"
    player <- getLine
    Prelude.putStrLn (show $ test )
    --play gameId player
