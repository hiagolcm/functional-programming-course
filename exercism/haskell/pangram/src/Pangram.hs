module Pangram (isPangram) where
import Data.Char (toLower)

toLowerCase :: String -> String
toLowerCase text = [toLower c | c <-text]

isPangram :: String -> Bool
isPangram text = if length alphabet == 26 then True else False
  where
    alphabet = [c | c <- ['a'.. 'z'], c `elem` lower_text]
    lower_text = toLowerCase text
