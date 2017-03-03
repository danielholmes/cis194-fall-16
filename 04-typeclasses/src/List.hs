module List where

data List a = Empty | Entry a (List a) deriving (Eq, Show)