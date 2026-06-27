mergeLists :: (Ord a) => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
mergeLists (x:xs) (y:ys)
  | x <= y = x : (mergeLists xs (y:ys))
  | y < x = y : (mergeLists (x:xs) ys) 

produtoCartesiano:: Num a => [a] -> [a] -> a
produtoCartesiano xs ys = sum [x*y | (x, y) <- zip xs ys]