module SumOfMultiples (sumOfMultiples) where

findMultiples :: Integer -> Integer -> [Integer]
findMultiples x y = [x * i | i <- [1 .. y], x * i < y]

mergeWithNoDuplications :: [Integer] -> [Integer] -> [Integer]
mergeWithNoDuplications xs [] = xs
mergeWithNoDuplications [] ys = ys
mergeWithNoDuplications xs (y : ys)
  | y `elem` xs = mergeWithNoDuplications xs ys
  | otherwise =  mergeWithNoDuplications (y : xs) ys

mergeListOfLists :: [[Integer]] -> [Integer]
mergeListOfLists []           = []
mergeListOfLists [x] = x
mergeListOfLists (x : y : []) = mergeWithNoDuplications x y
mergeListOfLists (x : y : xs) = mergeListOfLists ((mergeWithNoDuplications x y):xs)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples [] _ = 0
sumOfMultiples factors limit = sum $ mergeListOfLists $ [findMultiples f limit | f <- factors]