module DNA (nucleotideCounts, Nucleotide (..)) where

import Data.Map (Map, fromList)

isValid :: String -> Bool
isValid xs = length xs == length [x | x <- xs, x `elem` "ACGT"]

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs
  | isValid xs = Right (fromList [(A, countBy 'A' xs), (C, countBy 'C' xs), (G, countBy 'G' xs), (T, countBy 'T' xs)])
  | otherwise = Left "Error"
  where
    countBy c xs = sum [1 | x <- xs, c == x]
