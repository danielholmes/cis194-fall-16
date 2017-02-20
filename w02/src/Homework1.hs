{-# LANGUAGE OverloadedStrings #-}
module Homework1 (exercise1, adjacentMovableCoord, player, atCoord, pictureOfMaze, Coord (C), Direction (U, R, L, D), Tile) where

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

player :: Picture
player = head & body & leftLeg & rightLeg & arms
    where
        head = translated 0 0.15 (solidCircle 0.15)
        arms = path [(-0.2, -0.1), (0.2, -0.1)]
        body = path [(0, 0.2), (0, -0.2)]
        leftLeg = path [(0, -0.2), (-0.2, -0.4)]
        rightLeg = path [(0, -0.2), (0.2, -0.4)]

wall :: Picture
wall = colored black (solidRectangle 1 1)

ground :: Picture
ground = colored (grey 0.9) (solidRectangle 1 1)

storage :: Picture
storage = colored green (solidCircle 0.3) & ground

box :: Picture
box = rectangle 1 1 & colored brown (solidRectangle 1 1)

draw21times :: (Integer -> Picture) -> Picture
draw21times something = go (-10)
    where
        go :: Integer -> Picture
        go 11 = blank
        go n  = something n & go (n+1)

pictureOfMaze :: Picture
pictureOfMaze = draw21times (\r -> draw21times (\c -> drawTileAt (C r c)))

drawTile :: Tile -> Picture
drawTile Wall = wall
drawTile Ground = ground
drawTile Storage = storage
drawTile Box = box
drawTile Blank = blank

maze :: Coord -> Tile
maze (C x y)
    | abs x > 4  || abs y > 4  = Blank
    | abs x == 4 || abs y == 4 = Wall
    | x ==  2 && y <= 0        = Wall
    | x ==  3 && y <= 0        = Storage
    | x >= -2 && y == 0        = Box
    | otherwise                = Ground

drawTileAt :: Coord -> Picture
drawTileAt (C x y)
    | y < -10 || y > 10 || x < -10 || x > 10 = blank
    | otherwise = atCoord (C x y) (drawTile (maze (C x y)))

handleTime :: Double -> Coord -> Coord
handleTime _ c = c

adjacentMovableCoord :: Direction -> Coord -> Coord
adjacentMovableCoord d c = movableOrFallback proposed (maze proposed) c
    where
        movableOrFallback :: Coord -> Tile -> Coord -> Coord
        movableOrFallback p Ground _ = p
        movableOrFallback p Storage _ = p
        movableOrFallback _ _ c = c
        proposed = adjacentCoord d c

handleEvent :: Event -> Coord -> Coord
handleEvent (KeyPress key) c
    | key == "Right" = adjacentMovableCoord R c
    | key == "Up"    = adjacentMovableCoord U c
    | key == "Left"  = adjacentMovableCoord L c
    | key == "Down"  = adjacentMovableCoord D c
handleEvent _ c      = c

drawState :: Coord -> Picture
drawState c = atCoord c player & pictureOfMaze

exercise1 :: IO ()
exercise1 = interactionOf (C 1 (-3)) handleTime handleEvent drawState