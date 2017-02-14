module Homework2 (
    exercise2
) where

import CodeWorld

treeAngle :: Double
treeAngle = pi/10

tree :: Integer -> Picture -> Picture
tree 0 blossom = blossom
tree n blossom = translated 0 1 (rotated treeAngle (tree (n-1) blossom) &
    rotated (-treeAngle) (tree (n-1) blossom)) &
    path [(0,0),(0,1)]

blossomingTree :: Double -> Picture
blossomingTree t = tree 8 (colored (RGBA 1 1 0 t) (solidCircle (0.2 * t)))

treeController :: Double -> Picture
treeController t = blossomingTree (min 1 (t/10))

exercise2 :: IO ()
exercise2 = animationOf treeController