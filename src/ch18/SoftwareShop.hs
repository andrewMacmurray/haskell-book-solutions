module Ch18.SoftwareShop where

type Founded = Int
type Coders = Int

data SoftwareShop =
  Shop
    { founded :: Founded
    , programmers :: Coders
    } deriving (Eq, Show)

data FoundedError =
    NegativeYears Founded
  | TooManyYears Founded
  | NegativeCoders Coders
  | TooManyCoders Coders
  | TooManyCodersForYears Founded Coders
  deriving (Eq, Show)

validateFounded :: Int -> Either FoundedError Founded
validateFounded n
  | n < 0     = Left $ NegativeYears n
  | n > 500   = Left $ TooManyYears n
  | otherwise = Right n

validateCoders :: Int -> Either FoundedError Coders
validateCoders n
  | n < 0     = Left $ NegativeCoders n
  | n > 500   = Left $ TooManyCoders n
  | otherwise = Right n

validateShop :: Int -> Int -> Either FoundedError SoftwareShop
validateShop y p =
  if p > div y 10
    then Left $ TooManyCodersForYears y p
    else Right $ Shop y p

mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
mkSoftware years coders = do
  founded <- validateFounded years
  programmers <- validateCoders coders
  validateShop founded programmers
