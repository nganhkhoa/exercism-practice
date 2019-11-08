module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear year
    | mod year 4   /= 0 = False
    | mod year 400 == 0 = True
    | mod year 100 /= 0 = True
    | otherwise         = False
