module MyZip where


myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys


myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith fn [] _ = []
myZipWith fn _ [] = []
myZipWith fn (x:xs) (y:ys) = fn x y : myZipWith fn xs ys


-- same as myZip
myZip2 = myZipWith (,)
