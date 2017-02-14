module Lecture (
    redLightMain, greenLightMain, animationMain, spreadMain, wideTreeMain,
    thinTreeMain, treeAnimationMain
) where

import CodeWorld

topCircle :: Color -> Picture
topCircle c = colored c (translated 0 (-1.5) (solidCircle 1))

bottomCircle :: Color -> Picture
bottomCircle c = colored c (translated 0 1.5 (solidCircle 1))

frame :: Picture
frame = rectangle 2.5 5.5

trafficLight :: Bool -> Picture
trafficLight True = bottomCircle green & topCircle black & frame
trafficLight False = topCircle red & bottomCircle black & frame

spread :: Picture -> Double -> Integer -> Picture
spread _ _ 0 = blank
spread pic dx i = pic & translated dx 0 (spread pic dx (i-1))

-- 2 :: Integer to get around some pedantic warnings
-- come back to this when understand more
isRoundedEven :: Double -> Bool
isRoundedEven num = (round(num) `mod` (2 :: Integer)) == 0

trafficController :: Double -> Picture
trafficController t
  | isRoundedEven (t/3) = trafficLight True
  | otherwise           = trafficLight False

tree :: Double -> Integer -> Picture
tree _ 0 = blank
tree a n = path [(0,0),(0,1)] & translated 0 1 (
   rotated a (tree a (n-1)) & rotated (-a) (tree a (n-1)))

treeFoldController :: Double -> Picture
treeFoldController t = tree ((sin t) / 3) 8

animationMain :: IO ()
animationMain = animationOf trafficController

spreadMain :: IO ()
spreadMain = drawingOf (spread (trafficLight True) 3 5)

redLightMain :: IO ()
redLightMain = drawingOf (trafficLight False)

greenLightMain :: IO ()
greenLightMain = drawingOf (trafficLight True)

thinTreeMain :: IO ()
thinTreeMain = drawingOf (tree (pi/20) 8)

wideTreeMain :: IO ()
wideTreeMain = drawingOf (tree (pi/7) 8)

treeAnimationMain :: IO ()
treeAnimationMain = animationOf treeFoldController