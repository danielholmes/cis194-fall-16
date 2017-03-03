module Exercise4 where

import Sokoban
import List

type LevelNum = Integer

data LeveledState = LeveledState LevelNum Coord Direction (List Coord) deriving Eq

exercise4 :: IO ()
exercise4 = runInteraction (resetable (withUndo (withStartScreen sokoban)))