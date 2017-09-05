module Ch27.Exercises where

-- let x = 1
-- :sprint x = _

-- let x = ['1']
-- :sprint x = "1"

-- let x = [1]
-- :sprint x = _

-- let x = 1 :: Int
-- :sprint x = 1

-- let f = \x -> x
-- let x = f 1
-- :sprint x = _

-- let f :: Int -> Int; f = \x -> x
-- let x = f 1
-- :sprint = _


-- snd (undefined, 1)
-- prints 1

-- let x = undefined
-- let y = x `seq` 1 in (x, y)
-- forces exception

-- length $ [1..5] ++ undefined
-- forces exception, length has to evaluate undefined as a value

-- length $ [1..5] ++ [undefined]
-- prints 6, length only has to evaluate spine of list and not values

-- const 1 undefined
-- prints 1

-- const 1 (undefined `seq` 1)
-- prints 1

-- const undefined 1
-- forces exception

x = undefined
y = "blah"
-- forces exception
main = do
  print (snd (x, x `seq` y))
