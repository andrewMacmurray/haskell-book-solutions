module Person where

type Name = String
type Age = Integer
type ValidatePerson a = Either [InvalidPerson] a

data InvalidPerson
  = NameEmpty
  | AgeTooLow
  deriving (Eq, Show)

data Person = Person Name Age deriving Show

mkPerson :: Name -> Age -> Either InvalidPerson Person
mkPerson name age
  | name /= "" && age >= 0 = Right (Person name age)
  | name == ""             = Left NameEmpty
  | otherwise              = Left AgeTooLow

ageOkay :: Age -> Either [InvalidPerson] Age
ageOkay age =
  case age >= 0 of
    True  -> Right age
    False -> Left [AgeTooLow]

nameOkay :: Name -> Either [InvalidPerson] Name
nameOkay name =
  case name /= "" of
    True  -> Right name
    False -> Left [NameEmpty]

mkPerson' :: Name -> Age -> ValidatePerson Person
mkPerson' name age =
  mkPerson'' (nameOkay name) (ageOkay age)

mkPerson'' :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
mkPerson'' (Right name) (Right age)     = Right (Person name age)
mkPerson'' (Left badName) (Left badAge) = Left (badName ++ badAge)
mkPerson'' (Left badName) _             = Left badName
mkPerson'' _ (Left badAge)              = Left badAge
