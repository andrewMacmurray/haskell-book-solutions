module MyFilter where

mult3s :: Integral a => [a] -> [a]
mult3s xs = filter (\x -> rem x 3 == 0) xs

lengthMult3s = length . mult3s

filterWords :: String -> [String]
filterWords sentence =
  let
    removeWords = [ "the", "a", "an" ]
    validWords x = all (/= x) removeWords
  in
    filter validWords $ words sentence
