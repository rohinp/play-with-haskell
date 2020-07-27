module Chapter10 where 

import Safe
import Data.Maybe
import Data.Bifoldable
import Data.Char
import Data.List
import Data.Time

data DatabaseItem = DbString String
    | DbNumber Integer
    | DbDate UTCTime
    deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
    [ DbDate (UTCTime
    (fromGregorian 1911 5 1)
    (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello, world!"
    , DbDate (UTCTime
    (fromGregorian 1921 5 1)
    (secondsToDiffTime 34123))
    ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate xs = foldr f [] xs
    where 
        f (DbDate t) acc = t : acc
        f _ acc = acc


filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber xs = foldr f [] xs
    where 
        f (DbNumber d) acc = d : acc
        f _ acc = acc


mostRecent :: [DatabaseItem] -> UTCTime
mostRecent db = foldr max (head times) times 
    where times = filterDbDate db

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb db = (sumN / size)
    where nums = filterDbNumber db                                                   
          size :: Double
          size = fromIntegral $ length nums
          sumN :: Double
          sumN = fromIntegral $ sum (nums)