module Acronym (abbreviate) where

import Control.Monad
import Data.Char

abbreviate :: String -> String
abbreviate str = do
  join
  $ map (filter isUpper) -- because of HTML special case
  $ map decase
  $ words
  $ map sanitize str
  where
    sanitize :: Char -> Char
    sanitize c =
      case c of
        '\'' -> '!' -- won't affect result
        '-' -> ' '
        ',' -> ' '
        '_' -> ' '
        _ -> c

    capitalize :: String -> String
    capitalize "" = ""
    capitalize (x:xs) = toUpper x : xs

    allUpperSanitize :: String -> String
    allUpperSanitize "" = ""
    allUpperSanitize (x:xs) = x : (map toLower xs)

    decase :: String -> String
    decase w =
      case (all isUpper w) of
        True -> allUpperSanitize w
        _ -> capitalize w

