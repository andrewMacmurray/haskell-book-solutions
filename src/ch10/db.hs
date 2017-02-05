module Db where
import Data.Time

data DatabaseItem =
    DbString String
  | DbNumber Integer
  | DbDate UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
      (fromGregorian 1911 5 1)
      (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbNumber 9005
  , DbString "Hello, world!"
  , DbDate (UTCTime
      (fromGregorian 1921 5 1)
      (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr getDate []
  where
    getDate (DbDate date) xs = xs ++ [date]
    getDate _ xs = xs ++ []

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr getDbNumber []
  where
    getDbNumber (DbNumber n) xs = xs ++ [n]
    getDbNumber _ xs = xs ++ []

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = foldr (+) 0 . filterDbNumber

avDb :: [DatabaseItem] -> Double
avDb items =
    fromIntegral
  . (flip div) (length items)
  . fromIntegral
  . sumDb
  $ items
