{-# LANGUAGE OverloadedStrings #-}
module Homework where

import CodeWorld
--import Debug.Trace
import Data.Text

-- Lists

data List a = Empty | Entry a (List a) deriving (Show, Eq)

mapList :: (a -> b) -> List a -> List b
mapList _ Empty = Empty
mapList f (Entry c cs) = Entry (f c) (mapList f cs)

combine :: List Picture -> Picture
combine Empty = blank
combine (Entry p ps) = p & combine ps

appendList :: List a -> List a -> List a
appendList Empty list = list
appendList (Entry e next) list = Entry e (appendList next list)

containsList :: Eq a => a -> List a -> Bool
containsList _ Empty = False
containsList i (Entry x xs) = x == i || containsList i xs

replaceList :: Eq a => a -> a -> List a -> List a
replaceList _ _ Empty = error "Not found"
replaceList from to (Entry x xs) =
    if from == x then Entry to xs
    else Entry x (replaceList from to xs)

allList :: List Bool -> Bool
allList Empty = True
allList (Entry x xs) = x && allList xs


-- Coordinates


data Coord = C Integer Integer deriving Eq

data Direction = R | U | L | D

eqCoord :: Coord -> Coord -> Bool
eqCoord (C x1 y1) (C x2 y2) = x1 == x2 && y1 == y2

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord U (C x y) = C  x   (y+1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord D (C x y) = C  x   (y-1)

moveFromTo :: Coord -> Coord -> Coord -> Coord
moveFromTo = undefined


-- The maze

data Tile = Wall | Ground | Storage | Box | Blank deriving (Eq)

maze :: Coord -> Tile
maze (C x y)
    | abs x > 4  || abs y > 4  = Blank
    | abs x == 4 || abs y == 4 = Wall
    | x ==  2 && y <= 0        = Wall
    | x ==  3 && y <= 0        = Storage
    | x >= -2 && y == 0        = Box
    | otherwise                = Ground

noBoxMaze :: Coord -> Tile
noBoxMaze c
    | tile == Box = Ground
    | otherwise = tile
    where tile = maze c

mazeWithBoxes :: List Coord -> Coord -> Tile
mazeWithBoxes Empty coord = noBoxMaze coord
mazeWithBoxes (Entry boxCoord rest) coord = if (eqCoord boxCoord coord) then Box
                                            else mazeWithBoxes rest coord

isMovable :: Tile -> Bool
isMovable Storage = True
isMovable Ground = True
isMovable _ = False

isPushableBox :: Coord -> Direction -> List Coord -> Bool
isPushableBox coord dir bCoords = (containsList coord bCoords) && isMovable nextPropTile && not (containsList nextPropCoord bCoords)
    where
        nextPropCoord = adjacentCoord dir coord
        nextPropTile = noBoxMaze nextPropCoord

-- The state

data State = State Coord Direction (List Coord)


initialBoxes :: List Coord
initialBoxes = go (-10) (-10) Empty
    where
        go :: Integer -> Integer -> List Coord -> List Coord
        go 11 11 accu = accu
        go 11 y accu = go (-10) (y+1) accu
        go x y accu = case (maze (C x y)) of
            Box -> go (x+1) y (Entry (C x y) accu)
            _ -> go (x+1) y accu

initialState :: State
initialState = State (C 1 (-3)) D initialBoxes

isWon :: State -> Bool
isWon (State _ _ bCoords) = allList (mapList isOnStorage bCoords)

isOnStorage :: Coord -> Bool
isOnStorage c = noBoxMaze c == Storage

-- Event handling

tryPlayerPushBox :: State -> Direction -> State
tryPlayerPushBox (State pos _ bCoords) dir
    | isPushableBox propCoord dir bCoords = State propCoord dir (replaceList propCoord nextPropCoord bCoords)
    | otherwise = State pos dir bCoords
    where
        propCoord = adjacentCoord dir pos
        nextPropCoord = adjacentCoord dir propCoord

tryPlayerMove :: State -> Direction -> State
tryPlayerMove s@(State pos _ bCoords) dir
    | containsList propCoord bCoords = tryPlayerPushBox s dir
    | isMovable propTile = State propCoord dir bCoords
    | otherwise = State pos dir bCoords
    where
        propCoord = adjacentCoord dir pos
        propTile = noBoxMaze propCoord

handleKeyPress :: Text -> State -> State
handleKeyPress "Right" s = tryPlayerMove s R
handleKeyPress "Up" s = tryPlayerMove s U
handleKeyPress "Left" s = tryPlayerMove s L
handleKeyPress "Down" s = tryPlayerMove s D
handleKeyPress _ s = s

handleEvent :: Event -> State -> State
handleEvent e s
    | isWon s = s
    | (KeyPress key) <- e = handleKeyPress key s
    | otherwise = s

-- Drawing

wall, ground, storage, box :: Picture
wall =    colored (grey 0.4) (solidRectangle 1 1)
ground =  colored yellow     (solidRectangle 1 1)
storage = colored white (solidCircle 0.3) & ground
box =     colored brown      (solidRectangle 1 1)

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

pictureOfMaze :: Picture
pictureOfMaze = draw21times (\r -> draw21times (\c -> drawTileAt (C r c)))

draw21times :: (Integer -> Picture) -> Picture
draw21times something = go (-10)
  where
    go :: Integer -> Picture
    go 11 = blank
    go n  = something n & go (n+1)

drawTileAt :: Coord -> Picture
drawTileAt c = atCoord c (drawTile (noBoxMaze c))


atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic


player :: Direction -> Picture
player R = translated 0 0.3 cranium
         & path [(0,0),(0.3,0.05)]
         & path [(0,0),(0.3,-0.05)]
         & path [(0,-0.2),(0,0.1)]
         & path [(0,-0.2),(0.1,-0.5)]
         & path [(0,-0.2),(-0.1,-0.5)]
  where cranium = circle 0.18
                & sector (7/6*pi) (1/6*pi) 0.18
player L = scaled (-1) 1 (player R) -- Cunning!
player U = translated 0 0.3 cranium
         & path [(0,0),(0.3,0.05)]
         & path [(0,0),(-0.3,0.05)]
         & path [(0,-0.2),(0,0.1)]
         & path [(0,-0.2),(0.1,-0.5)]
         & path [(0,-0.2),(-0.1,-0.5)]
  where cranium = solidCircle 0.18
player D = translated 0 0.3 cranium
         & path [(0,0),(0.3,-0.05)]
         & path [(0,0),(-0.3,-0.05)]
         & path [(0,-0.2),(0,0.1)]
         & path [(0,-0.2),(0.1,-0.5)]
         & path [(0,-0.2),(-0.1,-0.5)]
  where cranium = circle 0.18
                & translated   0.06  0.08 (solidCircle 0.04)
                & translated (-0.06) 0.08 (solidCircle 0.04)

pictureOfBoxes :: List Coord -> Picture
pictureOfBoxes cs = combine (mapList (\c -> atCoord c (drawTile Box)) cs)

drawState :: State -> Picture
drawState s
    | isWon s = winOverlay & drawScene s
    | otherwise = drawScene s

drawScene :: State -> Picture
drawScene (State pCoord pDirection bCoords) = atCoord pCoord (player pDirection) & pictureOfBoxes bCoords & pictureOfMaze

winOverlay :: Picture
winOverlay = scaled 1.5 1.5 (text "You win!")

-- The complete interaction

sokoban :: Interaction State
sokoban = Interaction initialState (\_ c -> c) handleEvent drawState

-- The general interaction type

data Interaction world = Interaction
        world
        (Double -> world -> world)
        (Event -> world -> world)
        (world -> Picture)


runInteraction :: Interaction s -> IO ()
runInteraction (Interaction state0 step handle draw)
  = interactionOf state0 step handle draw

-- Resetable interactions

resetable :: Interaction s -> Interaction s
resetable (Interaction state0 step handle draw)
  = Interaction state0 step handle' draw
  where handle' (KeyPress key) _ | key == "Esc" = state0
        handle' e s = handle e s

-- Start screen

startScreen :: Picture
startScreen = scaled 3 3 (text "Sokoban!")

data SSState world = StartScreen | Running world

withStartScreen :: Interaction s  -> Interaction (SSState s)
withStartScreen (Interaction state0 step handle draw)
  = Interaction state0' step' handle' draw'
  where
    state0' = StartScreen

    step' _ StartScreen = StartScreen
    step' t (Running s) = Running (step t s)

    handle' (KeyPress key) StartScreen | key == " " = Running state0
    handle' _              StartScreen              = StartScreen
    handle' e              (Running s)              = Running (handle e s)

    draw' StartScreen = startScreen
    draw' (Running s) = draw s


-- The main function

homework :: IO ()
homework = runInteraction (resetable (withStartScreen sokoban))
