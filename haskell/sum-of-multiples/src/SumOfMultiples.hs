module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples [] _ = 0
sumOfMultiples [0] _ = 0
sumOfMultiples factors limit = foldl (\acc item ->
  case (any (== 0) (map (mod item) nonZeroFactors)) of
      True -> acc + item
      _ -> acc
    ) 0 [1..limit-1]
  where
    nonZeroFactors = filter (/= 0) factors
