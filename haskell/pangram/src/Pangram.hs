module Pangram (isPangram) where
import Data.Char
import Data.List

isPangram :: String -> Bool
isPangram input
    | text == "abcdefghijklmnopqrstuvwxyz" = True
    | otherwise = False
    where text = nub (sort (map toLower (filter isLetter input)))
