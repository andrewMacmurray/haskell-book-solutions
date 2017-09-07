module Ch29.INIDir where

import System.Directory (listDirectory)
import Text.Trifecta (parseFromFile)
import Ch24.Logfile hiding (main)
import qualified Data.Map as M
import Data.Maybe (catMaybes)

newtype Path = Path String deriving (Eq, Ord, Show)
newtype FileName = FileName String deriving (Eq, Ord, Show)

currentPath :: Path
currentPath = Path "./logs/"

main :: IO ()
main = do
  let (Path p) = currentPath
  xs <- listDirectory p
  let files :: IO [(Maybe (Maybe (FileName, [DayLog])))]
      files = traverse (sequenceA . iniFile currentPath) $ xs
  result <- files
  print . show . makeLogMap $ result

makeLogMap :: [Maybe (Maybe (FileName, [DayLog]))] -> M.Map FileName [DayLog]
makeLogMap = M.fromList . (catMaybes . catMaybes)

iniFile :: Path -> String -> Maybe (IO (Maybe (FileName, [DayLog])))
iniFile path fileName =
  case getExt fileName of
    "ini" -> Just $ parseDayLog path (FileName fileName)
    _     -> Nothing

getExt :: String -> String
getExt xs = drop (length xs - 3) xs

parseDayLog :: Path -> FileName -> IO (Maybe (FileName, [DayLog]))
parseDayLog (Path path) f@(FileName fileName) =
  (fmap . fmap) ((,) f) (parseFromFile logFile $ path ++ fileName)
