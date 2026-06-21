module Darts (score) where

score :: Float -> Float -> Int
score x y 
    | dist > 10 = 0
    | dist > 5 = 1
    | dist > 1 = 5
    | otherwise = 10
    where dist = sqrt (x * x + y * y)