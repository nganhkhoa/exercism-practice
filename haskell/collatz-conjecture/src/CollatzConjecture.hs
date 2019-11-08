module CollatzConjecture (collatz) where

import Prelude

import Data.Maybe (Maybe(..))
import Control.Applicative ((<*>))

collatz :: Integer -> Maybe Integer
collatz 1 = Just 0
collatz x
  | x <= 0 = Nothing
  | x `mod` 2 == 0 = Just (+) <*> Just 1 <*> collatz (div x 2)
  | otherwise = Just (+) <*> Just 1 <*> collatz (3*x + 1)
