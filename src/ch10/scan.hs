fibs = 1 : scanl (*) 1 : fibs

fibN x = fibs !! x

facs = scanl (*) 1 [2..]

facN x = facs !! x
