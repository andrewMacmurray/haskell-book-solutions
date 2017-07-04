module Ch22.BipBoop where

import Control.Applicative

boop :: Integer -> Integer
boop = (*2)

doop :: Integer -> Integer
doop = (+10)

bip :: Integer -> Integer
bip = boop . doop

bloop :: Integer -> Integer
bloop = fmap boop doop

-- used when you want to supply the same argument to both functions (boop and doop)
-- and then do something with the result of them afterwards

bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

-- a <- boop is taking away the (Integer ->) bit of boop
-- giving access to the final result of appling that Integer
-- a is *bound* to the *result* of applying an argument to boop

boopDoop :: Integer -> Integer
boopDoop = do
  a <- boop
  b <- doop
  return (a + b)
