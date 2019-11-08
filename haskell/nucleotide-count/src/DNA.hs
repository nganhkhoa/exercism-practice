module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map as Map
import Data.Either (Either(..))

data Nucleotide =
  A | C | G | T
  deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts "" = Right $ Map.fromList [ (A, 0)
                                           , (C, 0)
                                           , (G, 0)
                                           , (T, 0)
                                           ]
nucleotideCounts (x:xs) =
  Map.unionWith (+)
  <$> countOne x
  <*> nucleotideCounts xs
  where
    countOne :: Char -> Either String (Map Nucleotide Int)
    countOne 'A' = Right $ Map.fromList [ (A, 1) ]
    countOne 'C' = Right $ Map.fromList [ (C, 1) ]
    countOne 'G' = Right $ Map.fromList [ (G, 1) ]
    countOne 'T' = Right $ Map.fromList [ (T, 1) ]
    countOne c = Left $ "Incorrect Nucleotide [" <> [c] <> "]"