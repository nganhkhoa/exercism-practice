module Raindrops
  ( raindrops
  ) where

import Prelude

import Data.String (null)

raindrops :: Int -> String
raindrops num =
  raindrops' num 0
  $ raindrops' num 7
  $ raindrops' num 5
  $ raindrops' num 3 ""
  where
    raindrops' :: Int -> Int -> String -> String
    raindrops' x 3 ans | x `mod` 3 == 0 = ans <> "Pling"
    raindrops' x 5 ans | x `mod` 5 == 0 = ans <> "Plang"
    raindrops' x 7 ans | x `mod` 7 == 0 = ans <> "Plong"
    raindrops' x 3 ans = ans
    raindrops' x 5 ans = ans
    raindrops' x 7 ans = ans
    raindrops' x _ ans | null ans == true = show x
    raindrops' x _ ans = ans
