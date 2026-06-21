module Darts (score) where

score :: Float -> Float -> Int
score x y = if dist > 10 
    then 0
    else if dist > 5
        then 1
        else if dist > 1
            then 5
            else 10
    where dist = sqrt (x ^ 2 + y ^ 2) :: FLoat