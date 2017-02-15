module Spring13Homework2 (
    hanoi, hanoi4, performMoves, startPositions, boardToStr, largestDisk
) where

import Data.List

type Peg = String
type Move = (Peg, Peg)
type NumDisks = Integer
type Disk = Integer
type PopulatedPeg = (Peg, [Disk])
type HanoiBoard = [PopulatedPeg]

findPegs :: [Move] -> [Peg]
findPegs ms = sort (nub ((map (fst) ms) ++ (map (snd) ms)))

pegWidth :: NumDisks -> Integer
pegWidth 0 = 3
pegWidth n = (diskWidth n) + 2

diskWidth :: Disk -> Integer
diskWidth d = (1 + d * 2)

startPositions :: NumDisks -> [Peg] -> HanoiBoard
startPositions _ [] = []
startPositions n (p:ps) = (p, [0..(n-1)]) : map (\x -> (x, [])) ps

performMove :: HanoiBoard -> Move -> HanoiBoard
performMove p (from,to) = let (newP, d) = takeDisk p from
                          in putDisk newP to d

putDisk :: HanoiBoard -> Peg -> Disk -> HanoiBoard
putDisk [] _ _ = error "Peg not found"
putDisk (p:ps) to disk
    | fst p == to = let (peg, disks) = p in (peg, disk : disks) : ps
    | otherwise = p : putDisk ps to disk

takeDisk :: HanoiBoard -> Peg -> (HanoiBoard, Disk)
takeDisk [] _ = error "Peg not found"
takeDisk (p:ps) from
    | fst p == from = let (x:xs) = (snd p)
                      in (((fst p), xs) : ps, x)
    | otherwise = let (pegs, found) = takeDisk ps from
                  in (p : pegs, found)

performAllMoves :: HanoiBoard -> [Move] -> HanoiBoard
performAllMoves p [] = p
performAllMoves p (m:ms) = performAllMoves (performMove p m) ms

performMoves :: NumDisks -> [Move] -> HanoiBoard
performMoves n ms = performAllMoves (startPositions n (findPegs ms)) ms

boardToStr :: HanoiBoard -> String
boardToStr [] = ""
boardToStr b = let (p:ps) = b
               in if (concat (map snd b)) == [] then baseRow (pegWidth 0) else intercalate "\n" (pegToStr (largestDisk b) p)

pegToStr :: Disk -> PopulatedPeg -> [String]
pegToStr d p = pegToRowStr (d+1) (pegWidth d) p

pegToRowStr :: Integer -> Integer -> PopulatedPeg -> [String]
pegToRowStr 0 w p = [baseRow w]
pegToRowStr n w p = let (pn,ds) = p
                    in [if n <= fromIntegral (length ds) then diskRow (ds !! fromIntegral ((fromIntegral (length ds)) - n)) w else emptyRow w] ++
                        (pegToRowStr (n-1) w p)

baseRow :: Integer -> String
baseRow w = ' ' : (replicate (fromIntegral (w-2)) 'X') ++ " "

emptyRow :: Integer -> String
emptyRow w = let halfW = w `div` 2; empty = replicate (fromIntegral halfW) ' '
             in empty ++ "|" ++ empty

diskRow :: Disk -> Integer -> String
diskRow d w = let dw = diskWidth d; numEmpty = ((w - dw) `div` 2); empty = replicate (fromIntegral numEmpty) ' '
              in empty ++ (replicate (fromIntegral dw) '-') ++ empty

largestDisk :: HanoiBoard -> Disk
largestDisk [] = error "No disks"
largestDisk b = maximum (concat (map snd b))

hanoi :: NumDisks -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
    | n <= 0 = []
    | otherwise = (hanoi (n-1) a c b) ++ [(a, b)] ++ (hanoi (n-1) c b a)

hanoi4 :: NumDisks -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 n a b c d
    | n <= 0 = []
    | n <= 2 = (hanoi4 (n-1) a c b d) ++ [(a, b)] ++ (hanoi4 (n-1) c b a d)
    | n == 3 = [(a, c), (a, d), (a, b), (d, b), (c, b)]
    | otherwise = (hanoi4 (n-1) a c b d) ++ [(a, b)] ++ (hanoi4 (n-1) c b a d)

{-
TODO: hanoi for x pegs
 - hanoi above is the minimum case (one temp peg)
 - hand code the next case (2 temp pegs) then attempt to find pattern/algorithm
-}