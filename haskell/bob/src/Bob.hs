module Bob (responseFor) where

import Data.Char
import Data.List
import Control.Monad

data ExpressionType = Question | Normal

responseFor :: String -> String
responseFor str | all isSpace str = "Fine. Be that way!"
responseFor str =
    case (statementType, isShouting) of
        (Question, True) -> "Calm down, I know what I'm doing!"
        (Question, _) -> "Sure."
        (_, True) -> "Whoa, chill out!"
        (_, _) -> "Whatever."
    where
        onlyLetters :: String
        onlyLetters = filter isLetter str
        filtered :: String
        filtered = filter (liftM2 (||) isLetter (== '?')) str

        isShouting :: Bool
        isShouting = ("" /= onlyLetters) && (all isUpper $ onlyLetters)

        isQuestion :: String -> Bool
        isQuestion "" = False
        isQuestion s = isSuffixOf "?" s

        statementType :: ExpressionType
        statementType = case (isQuestion filtered) of
            True -> Question
            _ -> Normal
