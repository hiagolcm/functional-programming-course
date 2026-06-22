module Bob (responseFor) where

import Data.Char

responseFor :: String -> String
responseFor xs
  | null trimmed = "Fine. Be that way!"
  | isYell && isQuestion = "Calm down, I know what I'm doing!"
  | isQuestion = "Sure."
  | isYell = "Whoa, chill out!"
  | otherwise = "Whatever."
  where
    trimmed = [c | c <- xs, c /= ' ', c /= '\t', c /= '\r', c /= '\n']
    isYell = all isUpper letters && any isUpper letters
    isQuestion = last trimmed == '?'
    letters = filter isLetter xs
