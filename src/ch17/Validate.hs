module Ch17.Validate where

validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
  if (length s) > maxLen
  then Nothing
  else Just s

valiatePhone :: String -> Maybe String
valiatePhone n =
  if length n /= 11
  then Nothing
  else Just n

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)
newtype Phone = Phone String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName a = fmap Name $ validateLength 100 a

mkAddress :: String -> Maybe Address
mkAddress a = fmap Address $ validateLength 100 a

mkPhone :: String -> Maybe Phone
mkPhone n = fmap Phone $ valiatePhone n


data Person =
  Person Name Address Phone
  deriving (Eq, Show)

mkPerson :: String -> String -> String -> Maybe Person
mkPerson n a p =
  Person <$> mkName n <*> mkAddress a <*> mkPhone p
