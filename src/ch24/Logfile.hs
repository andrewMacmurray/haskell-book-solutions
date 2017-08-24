{-# LANGUAGE QuasiQuotes #-}

module Ch24.Logfile where

import Control.Applicative
import Text.Trifecta
import Text.RawString.QQ
import Data.Time
import qualified Data.Map as M
import Data.Map (Map)
import Safe (headMay, lastMay)

newtype Activity =
  Activity String
  deriving (Eq, Show)


data DayLog =
  DayLog Day (Map UTCTime Activity)
  deriving (Eq, Show)

logFileEx :: String
logFileEx = [r|
-- wheee a comment

# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep

# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]


totalTimePerDay :: DayLog -> Maybe NominalDiffTime
totalTimePerDay (DayLog _ activities) =
  let times = map fst . M.toList $ activities
      start = headMay times
      end   = lastMay times
  in
    liftA2 diffUTCTime end start


averageActivityTime :: [DayLog] -> Maybe NominalDiffTime
averageActivityTime daylogs =
    fmap (calcAvg . sum) $ traverse averageActivityTimePerDay daylogs
    where
      calcAvg total = total / (fromIntegral $ length daylogs)


averageActivityTimePerDay :: DayLog -> Maybe NominalDiffTime
averageActivityTimePerDay daylog@(DayLog _ activities) =
  let timeDiff = totalTimePerDay daylog
      numberOfActivities = fromIntegral . M.size $ activities
  in
    fmap (\t -> t / numberOfActivities) timeDiff


logFile :: Parser [DayLog]
logFile = some dayLog


dayLog :: Parser DayLog
dayLog = do
  _  <- skipNewlines
  _  <- skipMany comment
  d  <- dateLine
  xs <- some (activity d)
  _  <- skipNewlines
  return $ DayLog d (M.fromList xs)

activity :: Day -> Parser (UTCTime, Activity)
activity day = do
  h  <- integer
  _  <- char ':'
  m  <- integer
  xs <- activityContent
  _  <- skipNewlines
  let time = UTCTime day $ timeOfDay h m
      act  = Activity xs
  return $ (time, act)


activityContent :: Parser String
activityContent = do
  try (manyTill exceptNewLine comment) <|> many exceptNewLine

timeOfDay :: Integer -> Integer -> DiffTime
timeOfDay h m =
  60 * 60 * (fromInteger h) + 60 * (fromInteger m)


dateLine :: Parser Day
dateLine = do
  _ <- string "# "
  y <- integer
  _ <- char '-'
  m <- integer
  _ <- char '-'
  d <- integer
  _ <- skipMany comment
  return $ fromGregorian y (fromInteger m) (fromInteger d)


comment :: Parser ()
comment = do
  _ <- skipMany (oneOf " ")
  _ <- string "-- "
  _ <- skipMany exceptNewLine
  _ <- skipNewlines
  return ()


exceptNewLine :: Parser Char
exceptNewLine = noneOf "\n"

skipNewlines :: Parser ()
skipNewlines = skipMany (oneOf "\n")


main :: IO ()
main = do
  print $ parseString comment mempty "-- a comment \n"
  print $ parseString dateLine mempty "# 2025-02-05\n"
  print $ parseString (activity $ fromGregorian 2015 2 1) mempty "08:00 hello world"
  let parsedLog = parseString logFile mempty logFileEx
  print $ parsedLog
  print $ (fmap . fmap) totalTimePerDay parsedLog
  print $ (fmap . fmap) averageActivityTimePerDay parsedLog
  print $ fmap averageActivityTime parsedLog
