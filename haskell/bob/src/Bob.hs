module Bob (responseFor) where
import Data.Char

responseFor :: String -> String
responseFor xs
    | length s == 0 = "Whatever."
    | all (isSpace) s = "Fine. Be that way!"
    | length s > 1 && head s == '?' && all (isUpper) (tail s) = "Calm down, I know what I'm doing!"
    | head s == '?' = "Sure."
    | all (isUpper) s = "Whoa, chill out!"
    | otherwise = "Whatever."
    where s = if all (isSpace) xs
                    then ' ' : xs
                    else if last (filter (not . isSpace) xs) == '?'
                        then '?' : filter (isLetter) xs
                        else filter (isLetter) xs
