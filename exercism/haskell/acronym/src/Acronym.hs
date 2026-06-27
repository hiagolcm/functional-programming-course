module Acronym (abbreviate) where

import Data.Char (toUpper)

isUpper :: Char -> Bool
isUpper x = x `elem` ['A' .. 'Z']

isLetter :: Char -> Bool
isLetter x = x `elem` ['a' .. 'z'] || x `elem` ['A' .. 'Z']

isValidCharacter :: Char -> Bool
isValidCharacter x = isLetter x || x == ' ' || x == '-'

trimWord :: String -> String
trimWord xs = [if x == '-' then ' ' else x | x <- xs, isValidCharacter x]

addSpaceToCamel :: String -> String -> String
addSpaceToCamel xs [] = xs
addSpaceToCamel [] (y : ys) = addSpaceToCamel [y] ys
addSpaceToCamel xs (y : ys)
  | isUpper y && not (isUpper (last xs)) = addSpaceToCamel (xs ++ " " ++ [y]) ys
  | otherwise = addSpaceToCamel (xs ++ [y]) ys

abbreviate :: String -> String
abbreviate xs = [toUpper c | (c : _) <- extractedWords]
  where
    extractedWords = words $ addSpaceToCamel [] $ trimWord xs
