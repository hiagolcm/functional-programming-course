addVectors :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVectors x y =  ((fst x) + (fst y), (snd x) + (snd y))

addVectors' :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVectors' (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs