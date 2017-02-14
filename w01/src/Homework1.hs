module Homework1 (
    exercise1
) where

import CodeWorld

frame :: Picture
frame = rectangle 2.5 7.5

lightShape :: Picture
lightShape = solidCircle 1

coloredLightShape :: Color -> Picture
coloredLightShape c = colored c lightShape

singleLight :: Color -> Bool -> Picture
singleLight c True = coloredLightShape c
singleLight _ False = coloredLightShape black

redLight :: Bool -> Picture
redLight on = translated 0 2.5 (singleLight red on)

amberLight :: Bool -> Picture
amberLight on = singleLight orange on

greenLight :: Bool -> Picture
greenLight on = translated 0 (-2.5) (singleLight green on)

trafficLight :: Bool -> Bool -> Bool -> Picture
trafficLight redOn amberOn greenOn = redLight redOn & amberLight amberOn &
    greenLight greenOn & frame

trafficLightAnimation :: Integer -> Picture
trafficLightAnimation t
    | t < 5 = trafficLight False False True
    | t < 6 = trafficLight False True False
    | t < 9 = trafficLight True False False
    | otherwise = trafficLight True True False

trafficLightController :: Double -> Picture
trafficLightController t = trafficLightAnimation (round t `mod` 10)

exercise1 :: IO ()
exercise1 = animationOf trafficLightController