module Exercise1 where

import List

elemList :: Eq a => a -> List a -> Bool
elemList y (Entry x xs) = y == x || elemList y xs
elemList _ Empty = False

appendList :: List a -> List a -> List a
appendList Empty b = b
appendList a Empty = a
appendList (Entry x xs) y = Entry x (appendList xs y)

listLength :: List a -> Integer
listLength Empty = 0
listLength (Entry _ xs) = 1 + listLength xs

filterList :: (a -> Bool) -> List a -> List a
filterList _ Empty = Empty
filterList p (Entry x xs)
    | p x = Entry x (filterList p xs)
    | otherwise = filterList p xs

nth :: List a -> Integer -> a
nth Empty _ = error "list too short"
nth (Entry x _) 1 = x
nth (Entry _ xs) y = nth xs (y-1)