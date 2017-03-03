{-# LANGUAGE OverloadedStrings #-}
module Exercise2 (exercise2, handleTime2, handleEvent2, drawState2) where

import CodeWorld
import Exercise1

player2 :: Direction -> Picture
player2 U = rotated pi player
player2 R = rotated (pi / 2) player
player2 L = rotated ((-pi) / 2) player
player2 D = player

adjacentMovableCoord2 :: Direction -> Coord -> (Coord, Direction)
adjacentMovableCoord2 d c = ((adjacentMovableCoord d c), d)

handleTime2 :: Double -> (Coord, Direction) -> (Coord, Direction)
handleTime2 _ s = s

handleEvent2 :: Event -> (Coord, Direction) -> (Coord, Direction)
handleEvent2 (KeyPress key) (c, _)
    | key == "Right" = adjacentMovableCoord2 R c
    | key == "Up"    = adjacentMovableCoord2 U c
    | key == "Left"  = adjacentMovableCoord2 L c
    | key == "Down"  = adjacentMovableCoord2 D c
handleEvent2 _ s     = s

drawState2 :: (Coord, Direction) -> Picture
drawState2 (c, d) = atCoord c (player2 d) & pictureOfMaze

exercise2 :: IO ()
exercise2 = interactionOf ((C 1 (-3)), D) handleTime2 handleEvent2 drawState2