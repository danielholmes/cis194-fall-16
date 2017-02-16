module HanoiDisplay (largestDisk, boardToStr) where

import Data.List
import Spring13Homework2

largestDisk :: HanoiBoard -> Disk
largestDisk [] = error "No disks"
largestDisk b = maximum (concat (map snd b))

pegWidth :: NumDisks -> Integer
pegWidth 0 = 3
pegWidth n = (diskWidth n) + 2

diskWidth :: Disk -> Integer
diskWidth d = (1 + d * 2)

boardToStr :: HanoiBoard -> String
boardToStr [] = ""
boardToStr b
    | (concat (map snd b)) == [] = baseRow (pegWidth 0)
    | otherwise = intercalate "\n" (boardToStrs (largestDisk b) b)

boardToStrs :: Disk -> HanoiBoard -> [String]
boardToStrs _ [] = []
boardToStrs d b = let (p:ps) = b in joinElementWise (pegToStr d p) (boardToStrs d ps)

joinElementWise :: [String] -> [String] -> [String]
joinElementWise [] [] = []
joinElementWise (x:xs) [] = x : joinElementWise xs []
joinElementWise [] (y:ys) = y : joinElementWise [] ys
joinElementWise (x:xs) (y:ys) = (x ++ y) : joinElementWise xs ys

pegToStr :: Disk -> PopulatedPeg -> [String]
pegToStr d p = pegToRowStr (d+1) (pegWidth d) p

pegToRowStr :: Integer -> Integer -> PopulatedPeg -> [String]
pegToRowStr 0 w _ = [baseRow w]
pegToRowStr n w p = pegRowToStr n w (snd p) : (pegToRowStr (n-1) w p)

pegRowToStr :: Integer -> Integer -> [Disk] -> String
pegRowToStr n w ds
    | n <= fromIntegral (length ds) = diskRow (ds !! fromIntegral ((fromIntegral (length ds)) - n)) w
    | otherwise = emptyRow w

baseRow :: Integer -> String
baseRow w = charRow 'X' (w-2) w

emptyRow :: Integer -> String
emptyRow w = charRow '|' 1 w

diskRow :: Disk -> Integer -> String
diskRow d w = charRow '-' (diskWidth d) w

charRow :: Char -> Integer -> Integer -> String
charRow c d w = let halfW = (w - d) `div` 2; empty = replicate (fromIntegral halfW) ' '
                in empty ++ (replicate (fromIntegral d) c) ++ empty
