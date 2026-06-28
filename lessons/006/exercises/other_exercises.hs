getIdx :: [a] -> Int -> Maybe a
getIdx list idx
  | idx >= length list = Nothing
  | idx < 0 = Nothing
  | otherwise = Just (head $ drop idx list)

fatorial :: Int -> Int
fatorial n = product [1..n]

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' n (x:xs) = x : take' (n-1) xs

divisores :: Int -> [Int]
divisores n = [x | x <- [1..n], n `mod` x == 0]

isPrime :: Int -> Bool
isPrime x = (length $ divisores x) == 2

allPrimes :: Int -> [Int]
allPrimes n = [x | x <- [1..n], isPrime x]

adjacentes :: [a] -> [(a, a)]
adjacentes xs = zip xs (tail xs)

and' :: [Bool] -> Bool
and' (True:xs) = and xs
and' ([]) = True
and' _ = False

sorted :: (Ord a) => [a] -> Bool
sorted [] = True
sorted [x] = True
sorted (x:y:xs) 
  | x > y = False
  | otherwise = sorted (y:xs)
