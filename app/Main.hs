module Main where

import MExp
import HttpClient
import Game

main :: IO ()
main = do
    Prelude.putStrLn "Hello, enter game id"
    x <- getLine
    Prelude.putStrLn(show $ test 3)
