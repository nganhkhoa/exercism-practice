module BracketPush
  ( isPaired
  ) where

import Prelude

import Data.Maybe (Maybe(..), fromJust)
import Data.List (List(..), (:), foldl, length, head, tail, snoc, insertAt)
import Data.String.CodeUnits (toCharArray)
import Partial.Unsafe (unsafePartial)

data BracketType
  = Round
  | Square
  | Curly

derive instance eqBracketType :: Eq BracketType

type Bracket = {
  type :: BracketType,
  enclosed :: Boolean
}

parseChar :: Char -> Maybe Bracket
parseChar c
  | c == '{' = Just { type: Curly,  enclosed: false }
  | c == '(' = Just { type: Round,  enclosed: false }
  | c == '[' = Just { type: Square, enclosed: false }
  | c == ']' = Just { type: Square, enclosed: true  }
  | c == ')' = Just { type: Round,  enclosed: true  }
  | c == '}' = Just { type: Curly,  enclosed: true  }
  | otherwise = Nothing

stackBracket :: Maybe (List Bracket) -> Maybe Bracket -> Maybe (List Bracket)
stackBracket acc (Nothing) = acc
stackBracket (Nothing) (Just c) = Just (c : Nil)
stackBracket (Just acc) (Just c@{type: _, enclosed: false}) = insertAt 0 c acc
stackBracket (Just acc) (Just c@{type: _, enclosed: true}) =
  if (checkHead c listHead) then
    tail acc
  else
    Just (snoc acc (unsafePartial (fromJust listHead)))
  where
  listHead :: Maybe Bracket
  listHead = head acc
  checkHead :: Bracket -> Maybe Bracket -> Boolean
  checkHead ch (Just hd) = ch.type == hd.type
  checkHead _ _ = false


isPaired :: String -> Boolean
isPaired "" = true
isPaired expr =
  case stack of
    Nothing -> false
    (Just s) -> eq 0 $ length s
  where
    stack =
      foldl stackBracket Nothing
      $ map parseChar
      $ toCharArray expr
