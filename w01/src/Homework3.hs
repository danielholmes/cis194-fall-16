module Homework3 (exercise3) where

import CodeWorld

wall :: Picture
wall = colored black (solidRectangle 1 1)

ground :: Picture
ground = colored (grey 0.9) (solidRectangle 1 1)

storage :: Picture
storage = colored green (solidCircle 0.3) & ground

box :: Picture
box = rectangle 1 1 & colored brown (solidRectangle 1 1)

drawTile :: Integer -> Picture
drawTile 1 = wall
drawTile 2 = ground
drawTile 3 = storage
drawTile 4 = box
drawTile _ = blank

maze :: Integer -> Integer -> Integer
maze x y
    | abs x > 4  || abs y > 4  = 0
    | abs x == 4 || abs y == 4 = 1
    | x ==  2 && y <= 0        = 1
    | x ==  3 && y <= 0        = 3
    | x >= -2 && y == 0        = 4
    | otherwise                = 2

drawMazeRow :: Integer -> Integer -> Picture
drawMazeRow x y
    | y < -10 || y > 10 || x < -10 || x > 10 = blank
    | otherwise = translated (fromIntegral x) (fromIntegral y) (drawTile (maze x y)) & drawMazeRow (x+1) y

drawMaze :: Integer -> Picture
drawMaze y
    | y < -10 || y > 10 = blank
    | otherwise = drawMazeRow (-10) y & drawMaze (y+1)

pictureOfMaze :: Picture
pictureOfMaze = drawMaze (-10)

exercise3 :: IO ()
exercise3 = drawingOf pictureOfMaze