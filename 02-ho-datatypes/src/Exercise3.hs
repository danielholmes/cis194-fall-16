{-# LANGUAGE OverloadedStrings #-}
module Exercise3 (exercise3) where

import CodeWorld
import Exercise1
import Exercise2

resetableInteractionOf ::
    world ->
    (Double -> world -> world) ->
    (Event -> world -> world) ->
    (world -> Picture) ->
    IO ()
resetableInteractionOf w ht he d = interactionOf w ht handleEvent3 d
    where
        isEscape :: Event -> Bool
        isEscape (KeyPress key) = key == "Esc"
        isEscape _ = False

        handleEvent3 e s
            | isEscape(e) = w
            | otherwise = he e s

exercise3 :: IO ()
exercise3 = resetableInteractionOf ((C 1 (-3)), D) handleTime2 handleEvent2 drawState2