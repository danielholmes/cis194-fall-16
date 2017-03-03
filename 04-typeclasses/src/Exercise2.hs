module Exercise2 where

import Exercise1
import List

isGraphClosed :: Eq a => a -> (a -> List a) -> (a -> Bool) -> Bool
isGraphClosed initial adjacent isOk = isGraphClosedStep Empty (Entry initial Empty) adjacent isOk

isGraphClosedStep :: Eq a => List a -> List a -> (a -> List a) -> (a -> Bool) -> Bool
isGraphClosedStep _ Empty _ _ = True
isGraphClosedStep seen todo@(Entry x xs) adjacent isOk
    | isOk x = isGraphClosedStep (Entry x seen) (appendList xs unseenAdjacent) adjacent isOk
    | otherwise = False
    where
        seenAndToDo = appendList seen todo
        unseenAdjacent = filterList (\a -> not $ elemList a seenAndToDo) $ adjacent x