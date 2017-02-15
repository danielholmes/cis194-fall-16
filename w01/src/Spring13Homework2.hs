module Spring13Homework2 (
    hanoi, hanoi4, performMoves, startPositions, Disk, HanoiBoard, NumDisks, PopulatedPeg
) where

import Data.List

type Peg = String
type Move = (Peg, Peg)
type NumDisks = Integer
type Disk = Integer
type PopulatedPeg = (Peg, [Disk])
type HanoiBoard = [PopulatedPeg]

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

findPegs :: [Move] -> [Peg]
findPegs ms = sort (nub ((map (fst) ms) ++ (map (snd) ms)))

performAllMoves :: HanoiBoard -> [Move] -> HanoiBoard
performAllMoves p [] = p
performAllMoves p (m:ms) = performAllMoves (performMove p m) ms

performMoves :: NumDisks -> [Move] -> HanoiBoard
performMoves n ms = performAllMoves (startPositions n (findPegs ms)) ms

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