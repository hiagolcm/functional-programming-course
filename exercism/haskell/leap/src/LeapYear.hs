module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear year = if (year `mod` 400 == 0) || year `mod` 4 == 0 && year `mod` 100 /= 0
    then True else False