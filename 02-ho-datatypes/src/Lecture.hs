{-# LANGUAGE OverloadedStrings #-}
module Lecture (mazeMain, offsetMazeMain, interactionMain) where

import CodeWorld

data Tile = Wall | Ground | Storage | Box | Blank

data Direction = R | U | L | D

data Coord = C Integer Integer

initialCoord :: Coord
initialCoord = C 0 0

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord U (C x y) = C  x   (y+1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord D (C x y) = C  x   (y-1)

wall :: Picture
wall = colored black (solidRectangle 1 1)

ground :: Picture
ground = colored (grey 0.9) (solidRectangle 1 1)

storage :: Picture
storage = colored green (solidCircle 0.3) & ground

box :: Picture
box = rectangle 1 1 & colored brown (solidRectangle 1 1)

--draw21times :: forall a. (Eq a, Num a) => (a -> Picture) -> Picture
draw21times :: (Integer -> Picture) -> Picture
draw21times something = go (-10)
    where
        go :: Integer -> Picture
        go 11 = blank
        go n  = something n & go (n+1)

pictureOfMaze :: Picture
pictureOfMaze = draw21times (\r -> draw21times (\c -> drawTileAt r c))

drawTile :: Tile -> Picture
drawTile Wall = wall
drawTile Ground = ground
drawTile Storage = storage
drawTile Box = box
drawTile Blank = blank

maze :: Integer -> Integer -> Tile
maze x y
    | abs x > 4  || abs y > 4  = Blank
    | abs x == 4 || abs y == 4 = Wall
    | x ==  2 && y <= 0        = Wall
    | x ==  3 && y <= 0        = Storage
    | x >= -2 && y == 0        = Box
    | otherwise                = Ground

drawTileAt :: Integer -> Integer -> Picture
drawTileAt x y
    | y < -10 || y > 10 || x < -10 || x > 10 = blank
    | otherwise = translated (fromIntegral x) (fromIntegral y) (drawTile (maze x y))

interactionMain :: IO ()
interactionMain = interactionOf initialCoord handleTime handleEvent drawState

handleTime :: Double -> Coord -> Coord
handleTime _ c = c

handleEvent :: Event -> Coord -> Coord
handleEvent (KeyPress key) c
    | key == "Right" = adjacentCoord R c
    | key == "Up"    = adjacentCoord U c
    | key == "Left"  = adjacentCoord L c
    | key == "Down"  = adjacentCoord D c
handleEvent _ c      = c

drawState :: Coord -> Picture
drawState c = atCoord c pictureOfMaze


mazeMain :: IO ()
mazeMain = drawingOf pictureOfMaze


someCoord :: Coord
someCoord = adjacentCoord L (adjacentCoord L (adjacentCoord L initialCoord))

offsetMazeMain :: IO ()
offsetMazeMain = drawingOf (atCoord someCoord pictureOfMaze)