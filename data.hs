module TicTacToe.Messages.MExpr
where

{-
message to find out a winner
board:
+-+-+-+
|O| | |
+-+-+-+
|X|O|O|
+-+-+-+
|X|X| |
+-+-+-+
-}
message :: String
message = "l[m[\"x\"; 2; \"y\";  1;   \"v\";  \"x\"];   m[\"x\";   0;  \"y\";   0;   \"v\";  \"o\"]; m[\"x\"; 2;   \"y\";   0; \"v\"; \"x\"]; m[\"x\";   1; \"y\";   1; \"v\";  \"o\"]; m[\"x\"; 1;   \"y\";  0;   \"v\";  \"x\"];  m[\"x\";   1; \"y\"; 2; \"v\"; \"o\"]]"
