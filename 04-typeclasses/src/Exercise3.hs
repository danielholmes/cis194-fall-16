module Exercise3 where

import Exercise1
import Exercise2
import Mazes
import List

isClosed :: Maze -> Bool
isClosed (Maze start mazeMap) = case (mazeMap start) of
                                    Blank -> False
                                    Wall -> False
                                    Box -> False
                                    _ -> isGraphClosed start (adjacentNonWalls mazeMap) (isWalkableCoord mazeMap)

isWalkableCoord :: (Coord -> Tile) -> Coord -> Bool
isWalkableCoord mazeMap = isWalkableTile . mazeMap

isWalkableTile :: Tile -> Bool
isWalkableTile Blank = False
isWalkableTile _ = True

isWall :: Tile -> Bool
isWall Wall = True
isWall _ = False

adjacentNonWalls :: (Coord -> Tile) -> Coord -> List Coord
adjacentNonWalls mazeMap = filterList (not . isWall . mazeMap) . allAdjacent

allAdjacent :: Coord -> List Coord
allAdjacent c = mapList (offsetCoord c) allDirections

allDirections :: List Direction
allDirections = Entry U . Entry D . Entry L . Entry R $ Empty

mapList :: (a -> b) -> List a -> List b
mapList _ Empty = Empty
mapList f (Entry x xs) = Entry (f x) (mapList f xs)