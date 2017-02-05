module Vehicles where

data Price =
  Price Integer deriving (Eq, Show)

data Size =
  Size Integer deriving (Eq, Show)

data Manufacturer =
    Mini
  | Mazda
  | Tata
  | None
  deriving (Eq, Show)

data Airline =
    PapuAir
  | Catapults
  | TakeYourChances
  deriving (Eq, Show)

data Vehichle =
    Car Manufacturer Price
  | Plane Airline Size
  deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir (Size 30000)

isCar :: Vehichle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehichle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehichle] -> [Bool]
areCars = map isCar

getManu :: Vehichle -> Manufacturer
getManu (Car manu _) = manu
getManu (Plane _ _) = None
