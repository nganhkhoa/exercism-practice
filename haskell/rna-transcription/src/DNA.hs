module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA "" = Right ""
toRNA (x:xs) = (:) <$> toRNA' x <*> toRNA xs
  where
    toRNA' :: Char -> Either Char Char
    toRNA' c
      | c == 'C' = Right 'G'
      | c == 'G' = Right 'C'
      | c == 'T' = Right 'A'
      | c == 'A' = Right 'U'
      | otherwise = Left c
