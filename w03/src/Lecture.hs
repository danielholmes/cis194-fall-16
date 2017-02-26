{-# LANGUAGE OverloadedStrings #-}
module Lecture (lecture) where

import CodeWorld

data SSState world = StartScreen | Running world

data Tile = Wall | Ground | Storage | Box | Blank

data Direction = R | U | L | D

data Coord = C Integer Integer

data Interaction world = Interaction
    world
    (Double -> world -> world)
    (Event -> world -> world)
    (world -> Picture)

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
drawTileAt c@(C x y)
    | y < -10 || y > 10 || x < -10 || x > 10 = blank
    | otherwise = atCoord c (drawTile (maze c))

adjacentMovableCoord :: Direction -> Coord -> Coord
adjacentMovableCoord d c = movableOrFallback proposed (maze proposed) c
    where
        movableOrFallback :: Coord -> Tile -> Coord -> Coord
        movableOrFallback p Ground _ = p
        movableOrFallback p Storage _ = p
        movableOrFallback _ _ c = c
        proposed = adjacentCoord d c

directionalPlayer :: Direction -> Picture
directionalPlayer U = rotated pi player
directionalPlayer R = rotated (pi / 2) player
directionalPlayer L = rotated ((-pi) / 2) player
directionalPlayer D = player

handleTime :: Double -> (Coord, Direction) -> (Coord, Direction)
handleTime _ s = s

handleEvent :: Event -> (Coord, Direction) -> (Coord, Direction)
handleEvent (KeyPress key) (c, _)
    | key == "Right" = (adjacentMovableCoord R c, R)
    | key == "Up"    = (adjacentMovableCoord U c, U)
    | key == "Left"  = (adjacentMovableCoord L c, L)
    | key == "Down"  = (adjacentMovableCoord D c, D)
handleEvent _ s     = s

drawState :: (Coord, Direction) -> Picture
drawState (c, d) = atCoord c (directionalPlayer d) & pictureOfMaze

startScreen :: Picture
startScreen = scaled 3 3 (text "Sokoban!")

resetable :: Interaction s -> Interaction s
resetable (Interaction state0 step handle draw)
  = Interaction state0 step handle' draw
  where handle' (KeyPress key) _ | key == "Esc" = state0
        handle' e s = handle e s

withStartScreen :: Interaction s -> Interaction (SSState s)
withStartScreen (Interaction state0 step handle draw)
  = Interaction state0' step' handle' draw'
  where
    state0' = StartScreen

    step' _ StartScreen = StartScreen
    step' t (Running s) = Running (step t s)

    handle' (KeyPress key) StartScreen
         | key == " "                  = Running state0
    handle' _              StartScreen = StartScreen
    handle' e              (Running s) = Running (handle e s)

    draw' StartScreen = startScreen
    draw' (Running s) = draw s

runInteraction :: Interaction s -> IO ()
runInteraction (Interaction state step handle draw) = interactionOf state step handle draw

lecture :: IO ()
lecture = runInteraction (resetable (withStartScreen (Interaction (C 1 (-3), D) handleTime handleEvent drawState)))