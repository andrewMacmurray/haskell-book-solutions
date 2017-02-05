data Identity a =
  Identity a

instance Eq a => Eq (Identity a) where
  (Identity a) == (Identity a') = a == a'
