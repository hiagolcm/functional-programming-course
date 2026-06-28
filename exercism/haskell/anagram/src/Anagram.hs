module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

isAnagram :: String -> String -> Bool
isAnagram xs ys 
  | lowerTarget == lowerCandidate = False
  | otherwise = sort lowerTarget == sort lowerCandidate
  where
    lowerCandidate = map toLower ys
    lowerTarget = map toLower xs

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss = filter (isAnagram xs) xss 
