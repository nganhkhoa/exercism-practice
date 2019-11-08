module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune
            deriving (Eq)

ageOn :: Planet -> Float -> Float
ageOn planet seconds
  | planet == Earth = yearOnEarth
  | planet == Mercury = yearOnEarth / 0.2408467
  | planet == Venus = yearOnEarth / 0.61519726
  | planet == Mars = yearOnEarth / 1.8808158
  | planet == Saturn = yearOnEarth / 29.447498
  | planet == Uranus = yearOnEarth / 84.016846
  | planet == Neptune = yearOnEarth / 164.79132
  | planet == Jupiter = yearOnEarth / 11.862615
  | otherwise = 0
  where yearOnEarth = seconds / 31557600
