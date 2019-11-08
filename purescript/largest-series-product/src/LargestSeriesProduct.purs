module LargestSeriesProduct
  ( largestProduct
  ) where

import Prelude

import Data.Maybe (Maybe(..), fromJust)
import Partial.Unsafe (unsafePartial)
import Data.List (List(..), length, foldl, snoc, tail)
import Data.Tuple (Tuple(..), snd)
import Data.String as String
import Data.String (Pattern(..), split)
import Data.Int (fromString)

largestProduct :: String -> Int -> Maybe Int
largestProduct "" 0 = Just 1
largestProduct _ 0 = Just 1
largestProduct "" _ = Nothing
largestProduct _ len | len < 0 = Nothing
largestProduct str len | (String.length str) < len = Nothing
largestProduct str len =
  snd
  $ foldl (findLargest) (Tuple Nil (Just 0))
  $ map fromString
  $ split (Pattern "") str
  where
    findLargest
      :: Tuple (List Int) (Maybe Int)
      -> Maybe Int
      -> Tuple (List Int) (Maybe Int)
    findLargest (Tuple lst _) Nothing = Tuple lst Nothing
    findLargest (Tuple lst Nothing) _ = Tuple lst Nothing
    findLargest (Tuple lst cur) num = do
      let
        n = unsafePartial $ fromJust num
        c = unsafePartial $ fromJust cur
        chain = snoc lst n
        prod = foldl (*) 1 chain
        breakList = tail chain

      let
        cmp1 = length chain `compare` len
        cmp2 = prod `compare` c
      case (Tuple cmp1 cmp2) of
        Tuple LT _ -> Tuple chain cur
        Tuple _ GT -> Tuple (unsafePartial $ fromJust breakList) (Just prod)
        Tuple _ _ -> Tuple (unsafePartial $ fromJust breakList) cur