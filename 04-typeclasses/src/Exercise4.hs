{-# LANGUAGE OverloadedStrings #-}
module Exercise4 where

import CodeWorld
import List
import Mazes
import Exercise1

mapList :: (a -> b) -> List a -> List b
mapList _ Empty = Empty
mapList f (Entry c cs) = Entry (f c) (mapList f cs)

combine :: List Picture -> Picture
combine Empty = blank
combine (Entry p ps) = p & combine ps

allList :: List Bool -> Bool
allList Empty = True
allList (Entry b bs) = b && allList bs

-- Coordinates

eqCoord :: Coord -> Coord -> Bool
eqCoord (C x1 y1) (C x2 y2) = x1 == x2 && y1 == y2


adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord U (C x y) = C  x   (y+1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord D (C x y) = C  x   (y-1)

moveFromTo :: Eq a => a -> a -> a -> a
moveFromTo c1 c2 c | c1 == c   = c2
                   | otherwise = c


-- The maze

type MazeMap = Coord -> Tile

noBoxMaze :: MazeMap -> Coord -> Tile
noBoxMaze m c = case m c of
  Box -> Ground
  t   -> t

mazeWithBoxes :: MazeMap -> List Coord -> Coord -> Tile
mazeWithBoxes m Empty c'        = noBoxMaze m c'
mazeWithBoxes m (Entry c cs) c'
  | eqCoord c c' = Box
  | otherwise  = mazeWithBoxes m cs c'

isOnStorage :: MazeMap -> Coord -> Bool
isOnStorage mazeMap c = case mazeMap c of Storage -> True
                                          _       -> False

gameWon :: State -> List Coord -> Bool
gameWon (State l _ _ _) cs = l == listLength mazes && levelWon (nthMazeMap l) cs

nonLastLevelWon :: State -> List Coord -> Bool
nonLastLevelWon (State l _ _ _) cs = l < listLength mazes && levelWon (nthMazeMap l) cs

levelWon :: MazeMap -> List Coord -> Bool
levelWon m cs = allList (mapList (isOnStorage m) cs)

nthMaze :: Integer -> Maze
nthMaze = nth mazes

nthMazeMap :: Integer -> (Coord -> Tile)
nthMazeMap n = mazeMap where (Maze _ mazeMap) = nthMaze n


-- The state

type LevelNum = Integer
data State = State LevelNum Coord Direction (List Coord) deriving Eq


initialBoxes :: MazeMap -> List Coord
initialBoxes m = go (-10) (-10)
  where
    go 11 11 = Empty
    go x  11 = go (x+1) (-10)
    go x  y  = case m (C x y) of
      Box -> Entry (C x y) (go x (y+1))
      _   ->                go x (y+1)

initialState :: State
initialState = loadLevel 1

loadLevel :: Integer -> State
loadLevel n = State n (C 0 1) R (initialBoxes mazeMap)
                where (Maze s mazeMap) = nthMaze n

-- Event handling

tryGoTo :: State -> Direction -> State
tryGoTo (State level from _ bx) d
  = case currentMaze to of
    Box -> case currentMaze beyond of
      Ground -> movedState
      Storage -> movedState
      _ -> didn'tMove
    Ground -> movedState
    Storage -> movedState
    _ -> didn'tMove
  where to          = adjacentCoord d from
        beyond      = adjacentCoord d to
        currentMaze = mazeWithBoxes m bx
        movedState  = State level to d movedBx
        movedBx     = mapList (moveFromTo to beyond) bx
        didn'tMove  = State level from d bx -- Yes, ' may be part of an identifier
        m = nthMazeMap level

handleEvent :: Event -> State -> State
handleEvent _ s@(State _ _ _ bx)
    | gameWon s bx = s
handleEvent (KeyPress key) s
    | key == "Right" = tryGoTo s R
    | key == "Up"    = tryGoTo s U
    | key == "Left"  = tryGoTo s L
    | key == "Down"  = tryGoTo s D
handleEvent _ s    = s

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

pictureOfMaze :: MazeMap -> Picture
pictureOfMaze m = draw21times (\r -> draw21times (\c -> drawTileAt m (C r c)))

draw21times :: (Integer -> Picture) -> Picture
draw21times something = go (-10)
  where
    go :: Integer -> Picture
    go 11 = blank
    go n  = something n & go (n+1)

drawTileAt :: MazeMap -> Coord -> Picture
drawTileAt m c = atCoord c (drawTile (noBoxMaze m c))


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


showWin :: State -> List Coord -> Picture
showWin s cs | gameWon s cs = scaled 2 2 (text "You won!")
             | otherwise    = blank

showLevelDone :: State -> List Coord -> Picture
showLevelDone s cs | nonLastLevelWon s cs = scaled 2 2 (text "Level done!")
                   | otherwise            = blank

pictureOfBoxes :: List Coord -> Picture
pictureOfBoxes cs = combine (mapList (\c -> atCoord c (drawTile Box)) cs)

drawState :: State -> Picture
drawState s@(State level c d boxes)
  = showWin s boxes
  & showLevelDone s boxes
  & atCoord c (player d)
  & pictureOfBoxes boxes
  & pictureOfMaze (nthMazeMap level)

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

instance Eq s => Eq (SSState s) where
  StartScreen == StartScreen = True
  Running s == Running s' = s == s'
  _ == _ = False

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

-- Undoable interactions

-- We need to remember the current state, and all past states:

data WithUndo a = WithUndo a (List a)

withUndo :: Eq a => Interaction a -> Interaction (WithUndo a)
withUndo (Interaction state0 step handle draw)
  = Interaction state0' step' handle' draw'
  where
    state0' = WithUndo state0 Empty

    step' t (WithUndo s stack) = WithUndo (step t s) stack

    handle' (KeyPress key) (WithUndo s stack) | key == "U"
      = case stack of Entry s' stack' -> WithUndo s' stack'
                      Empty           -> WithUndo s Empty
    handle' e              (WithUndo s stack)
       | s' == s = WithUndo s stack
       | otherwise = WithUndo s' (Entry s stack)
      where s' = handle e s

    draw' (WithUndo s _) = draw s


-- The main function

exercise4 :: IO ()
exercise4 = runInteraction (resetable (withUndo (withStartScreen sokoban)))