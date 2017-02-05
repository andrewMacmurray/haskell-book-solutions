module RegisteredUser where

newtype Username = Username String
newtype AccountNumber = AccountNumber Integer

data User =
    UnregisteredUser
  | RegisteredUser Username AccountNumber

user1 :: User
user1 = RegisteredUser
  (Username "Andrew")
  (AccountNumber 12345)


printUser :: User -> IO ()
printUser UnregisteredUser =
  putStrLn "Unregistered user"

printUser (RegisteredUser (Username name) (AccountNumber number)) =
  putStrLn $ name ++ " " ++ show number

f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, _, c) (d, _, f) = ((a, d), (c, f))

functionC x y =
  case z of
    True -> x
    False -> y
  where z = x > y

isEvenAdd2 n =
  case z of
    True  -> n + 2
    False -> n
  where
    z = even n

nums x =
  case (compare x 0) of
    LT -> -1
    GT -> 1
    EQ -> 0
