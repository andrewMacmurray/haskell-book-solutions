module Deconstruct where

newtype Name = Name String deriving Show
newtype Acres   = Acres Int deriving Show

data FarmerType =
    DairyFarmer
  | WheatFarmer
  | SoybeanFarmer deriving Show

data FarmerRec =
  FarmerRec
    { name :: Name
    , acres :: Acres
    , farmerType :: FarmerType }
    deriving Show


-- each field in a record can be used as a function
-- to return the value for that field for a given record
isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmer = case farmerType farmer of
  DairyFarmer -> True
  _           -> False
