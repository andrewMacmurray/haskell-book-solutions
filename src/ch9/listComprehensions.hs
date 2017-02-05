module ListComp where

myList = [ x^2 | x <- [1..100], rem x 2 == 0 ]

mySecondList = [ x^y | x <- [1..10], y <- [ 2, 3 ], x^y < 50 ]

myThirdList = [ x^2 | x <- mySecondList, x < 300 ]

mySqr = [ x^2 | x <- [1..5] ]
