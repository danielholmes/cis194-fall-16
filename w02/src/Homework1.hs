{-# LANGUAGE OverloadedStrings #-}
module Homework1 (exercise1, exercise2, exercise3) where

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

player2 :: Direction -> Picture
player2 U = rotated pi player
player2 R = rotated (pi / 2) player
player2 L = rotated ((-pi) / 2) player
player2 D = player

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

exercise1 :: IO ()
exercise1 = interactionOf (C 1 (-3)) handleTime handleEvent drawState

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

resetableInteractionOf ::
    world ->
    (Double -> world -> world) ->
    (Event -> world -> world) ->
    (world -> Picture) ->
    IO ()
resetableInteractionOf w ht he d = interactionOf w ht handleEvent3 d
    where
        isEscape :: Event -> Bool
        isEscape (KeyPress key) = key == "Esc"
        isEscape _ = False

        handleEvent3 e s
            | isEscape(e) = w
            | otherwise = he e s

exercise3 :: IO ()
exercise3 = resetableInteractionOf ((C 1 (-3)), D) handleTime2 handleEvent2 drawState2