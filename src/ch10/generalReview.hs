module GeneralReview where

stops = "pbtdkg"
vowels = "aeiou"

nouns =
  [ "apple"
  , "orange"
  , "lemon"
  ]

verbs =
  [ "eat"
  , "peel"
  , "pluck"
  ]

combinations :: [a] -> [a] -> [(a, a, a)]
combinations stops vowels =
  let
    x = head stops
    y = last vowels
    start = map (\v -> (x, v, y)) vowels
    go ys (x:[]) = []
    go ys (x:y:xs) = map (\v -> (x, v, y)) ys ++ go ys (y:xs)
  in
    start ++ go stops vowels

onlyP = filter (\(a, _, _) -> a == 'p') . combinations vowels $ stops


-- gets the average characters per word in a string
seekritFunc x =
  (/) (fromIntegral (sum (map length (words x))))
      (fromIntegral (length (words x)))
