module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA ""     = Right ""
toRNA "G" = Right "C"
toRNA "C" = Right "G"
toRNA "T" = Right "A"
toRNA "A" = Right "U"
toRNA [x] = Left x
toRNA (x:xs) = case (toRNA [x]) of
    Left c -> Left c
    Right r  -> case toRNA xs of
        Left c   -> Left c
        Right rs -> Right (r ++ rs)
